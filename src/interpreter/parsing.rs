use crate::{
    error::{code_pos::CodePos, FenderError, FenderResult},
    operators::FenderBinaryOperator,
    operators::FenderInitializer,
    operators::FenderUnaryOperator,
    type_sys::{
        fender_value::FenderValue, freight_type_system::FenderTypeSystem, type_id::FenderTypeId,
    },
    unwrap_rust,
};
use flux_bnf::tokens::iterators::SelectTokens;
use flux_bnf::tokens::Token;
use freight_vm::function::FunctionRef;
use freight_vm::{
    execution_engine::ExecutionEngine,
    expression::{Expression, VariableType},
    function::{ArgCount, FunctionType, FunctionWriter},
};
use std::{
    collections::{HashMap, HashSet},
    error::Error,
    rc::Rc,
};

use super::{
    code_body_uses_lambda_parameter,
    error::InterpreterError,
    lexical_scope::LexicalScope,
    module_loading::{parse_import, parse_plugin},
    register_struct, register_var, Exports, InterpreterResult, RegisterVarType,
};

pub(crate) fn parse_main_function(
    token: &Token,
) -> FenderResult<(
    ExecutionEngine<FenderTypeSystem>,
    FunctionRef<FenderTypeSystem>,
)> {
    let mut engine: ExecutionEngine<FenderTypeSystem> = ExecutionEngine::new_default();
    let mut main = FunctionWriter::new(ArgCount::new(..));
    let main_return_target = engine.create_return_target();
    let mut scope = LexicalScope::new(ArgCount::Fixed(0), main_return_target);
    let labels = token
        .rec_iter()
        .select_token("label")
        .map(|t| t.children[0].get_match())
        .collect::<HashSet<_>>()
        .into_iter()
        .enumerate()
        .map(|(i, name)| (name, i))
        .collect();

    scope.labels = Rc::new(labels);
    for statement in parse_statements(
        &token.children,
        &mut engine,
        &mut scope,
        RegisterVarType::Global,
    )? {
        main.evaluate_expression(statement);
    }
    let main_ref = engine.register_function(main, main_return_target);
    Ok((engine, main_ref))
}

pub(crate) fn parse_args(token: &Token) -> (Vec<String>, Vec<String>, Option<String>) {
    let mut arg_names = Vec::new();
    let mut optional_arg_names = Vec::new();
    for arg in token.children_named("arg") {
        if arg.children.len() == 2 {
            unimplemented!();
        }
        let name = arg.children[0].get_match();
        arg_names.push(name);
    }
    if let Some(args) = token.children_named("optionalArgs").next() {
        for arg in args.children.iter() {
            match arg.get_name().as_deref().unwrap() {
                "arg" => optional_arg_names.push(arg.get_match()),
                "vararg" => {
                    let varg_name = optional_arg_names.pop();
                    return (arg_names, optional_arg_names, varg_name);
                }
                e => unreachable!("{:?}", e),
            }
        }
    }
    (arg_names, optional_arg_names, None)
}

pub(crate) fn parse_closure(
    token: &Token,
    engine: &mut ExecutionEngine<FenderTypeSystem>,
    scope: &mut LexicalScope,
) -> FenderResult<Expression<FenderTypeSystem>> {
    let function_ref = match token.matcher_name.as_deref().unwrap() {
        "closure" if token.children.len() == 2 => {
            let args = &token.children[0];
            let code_body = &token.children[1];

            let (args, optional_args, varg_name) = parse_args(args);

            let arg_count = match (optional_args.len(), &varg_name) {
                (op_arg_count, Some(_)) => ArgCount::Variadic {
                    min: args.len(),
                    max: args.len() + op_arg_count,
                },
                (op_arg_count, None) if op_arg_count > 0 => ArgCount::Range {
                    min: args.len(),
                    max: args.len() + op_arg_count,
                },
                _ => ArgCount::Fixed(args.len()),
            };

            let mut new_scope = scope.child_scope(arg_count, engine.create_return_target());
            for (index, arg) in args
                .into_iter()
                .chain(optional_args.into_iter())
                .chain(varg_name)
                .enumerate()
            {
                new_scope
                    .variables
                    .borrow_mut()
                    .insert(arg, VariableType::Stack(index));
            }
            parse_code_body(code_body, engine, &mut new_scope)
        }
        "closure" | "codeBody" => {
            let code_body = token
                .matcher_name
                .as_deref()
                .map(|n| n == "codeBody")
                .and_then(|b| b.then_some(token))
                .unwrap_or_else(|| &token.children[0]);
            let arg_count = if code_body_uses_lambda_parameter(code_body) {
                ArgCount::Fixed(1)
            } else {
                ArgCount::Range { min: 0, max: 1 }
            };
            let mut new_scope = scope.child_scope(arg_count, engine.create_return_target());
            parse_code_body(code_body, engine, &mut new_scope)
        }
        _ => unreachable!(),
    }?;
    Ok(match &function_ref.function_type {
        FunctionType::CapturingDef(_) => Expression::FunctionCapture(function_ref),
        _ => FenderValue::Function(function_ref).into(),
    })
}

pub(crate) fn parse_code_body(
    token: &Token,
    engine: &mut ExecutionEngine<FenderTypeSystem>,
    new_scope: &mut LexicalScope,
) -> FenderResult<FunctionRef<FenderTypeSystem>> {
    let mut function = FunctionWriter::new(new_scope.args);
    for expr in parse_statements(&token.children, engine, new_scope, RegisterVarType::Stack)? {
        function.evaluate_expression(expr);
    }
    let captures = std::mem::take(&mut *new_scope.captures.borrow_mut());
    if !captures.is_empty() {
        function.set_captures(captures);
    }
    new_scope.register_stack_vars(&mut function);
    function.layout = new_scope.stack_layout.borrow().clone();
    Ok(engine.register_function(function, new_scope.return_target))
}

pub(crate) fn parse_exports(
    token: &Token,
    engine: &mut ExecutionEngine<FenderTypeSystem>,
    scope: &mut LexicalScope,
    registration_type: RegisterVarType,
) -> FenderResult<Vec<Expression<FenderTypeSystem>>> {
    if token.children.is_empty() {
        scope.exports = Exports::All;
        return Ok(vec![]);
    }
    let names_before: HashSet<_> = scope.variables.borrow().keys().cloned().collect();
    let code_body = &token.children[0];
    let exprs = parse_statements(&code_body.children, engine, scope, registration_type)?;
    let exports = Exports::Some(
        scope
            .variables
            .borrow()
            .keys()
            .filter(|&s| !names_before.contains(s))
            .cloned()
            .collect(),
    );
    scope.exports = exports;
    Ok(exprs)
}

pub(crate) fn parse_statements(
    statements: &[Token],
    engine: &mut ExecutionEngine<FenderTypeSystem>,
    scope: &mut LexicalScope,
    registration_type: RegisterVarType,
) -> FenderResult<Vec<Expression<FenderTypeSystem>>> {
    let mut exprs = vec![];
    for token in statements {
        let token = &token.children[0];
        let expr = match token.get_name().as_deref().unwrap() {
            "expr" => parse_expr(token, engine, scope)?,
            "assignment" => parse_assignment(token, engine, scope)?,
            "return" => parse_return(token, engine, scope)?,
            "declaration" => {
                let name = token.children[0].get_match();
                let expr = &token.children[1];
                register_var(
                    name,
                    registration_type,
                    engine,
                    scope,
                    |engine, scope| parse_expr(expr, engine, scope),
                    token.range.start,
                )?
            }
            "structDeclaration" => {
                parse_struct_declaration(token, engine, scope, registration_type)?
            }
            "export" => {
                exprs.extend(parse_exports(token, engine, scope, registration_type)?);
                continue;
            }
            "import" => {
                parse_import(token, engine, scope, registration_type)?;
                continue;
            }
            "plugin" => {
                parse_plugin(token, engine, scope)?;
                continue;
            }
            name => unreachable!("{name}"),
        };
        exprs.push(expr);
    }
    Ok(exprs)
}

pub(crate) fn parse_struct_declaration(
    token: &Token,
    engine: &mut ExecutionEngine<FenderTypeSystem>,
    scope: &mut LexicalScope,
    registration_type: RegisterVarType,
) -> InterpreterResult {
    let mut struct_name = String::new();
    let mut fields = Vec::new();

    for child_token in token.iter() {
        match child_token.get_name().as_deref().unwrap() {
            "name" => struct_name = child_token.get_match(),
            "structBody" => {
                for arg_token in child_token.iter() {
                    let name = arg_token.children[0].get_match();

                    let type_check = if arg_token.children.len() > 1 {
                        FenderTypeId::type_from_str(&arg_token.children[1].children[0].get_match())
                    } else {
                        None
                    };

                    fields.push((name, type_check));
                }
            }
            e => unreachable!("{}", e),
        }
    }

    register_struct(
        struct_name,
        fields,
        engine,
        scope,
        registration_type,
        token.range.start,
    )
}

pub(crate) fn parse_assignment(
    token: &Token,
    engine: &mut ExecutionEngine<FenderTypeSystem>,
    scope: &mut LexicalScope,
) -> InterpreterResult {
    let target = &token.children[0];
    let value = &token.children[token.children.len() - 1];
    let op = token
        .children_named("assignOp")
        .next()
        .map(|t| parse_binary_operator(&t.get_match()));
    let target_expr = parse_expr(target, engine, scope)?;
    if let Expression::Variable(v) = &target_expr {
        if let VariableType::Stack(s) = v {
            scope.stack_layout.borrow_mut().set_alloc(*s);
        } else {
            scope.mark_mutable(
                &target
                    .rec_iter()
                    .select_token("name")
                    .next()
                    .expect(
                        "Left-hand assignment expression did not have a name to mark as mutable",
                    )
                    .get_match(),
            );
        }
    }
    let value = parse_expr(value, engine, scope)?;
    Ok(if let Some(op) = op {
        Expression::BinaryOpEval(
            FenderBinaryOperator::AssignOperate(op.into()),
            [target_expr, value].into(),
        )
    } else {
        Expression::AssignDynamic([target_expr, value].into())
    })
}

pub(crate) fn parse_return(
    token: &Token,
    engine: &mut ExecutionEngine<FenderTypeSystem>,
    scope: &mut LexicalScope,
) -> InterpreterResult {
    let name = token.children_named("name").next().map(|t| t.get_match());
    let expr = token.children_named("expr").next();
    let expr = if let Some(expr) = expr {
        parse_expr(expr, engine, scope)?
    } else {
        FenderValue::Null.into()
    };
    if let Some(name) = name {
        let label = scope
            .labels
            .get(&name)
            .ok_or(InterpreterError::UnresolvedLabel(name, token.range.start))?;
        Ok(Expression::Return(*label, expr.into()))
    } else {
        Ok(Expression::Return(scope.top_level_return(), expr.into()))
    }
}

pub(crate) fn parse_binary_operator(op: &str) -> FenderBinaryOperator {
    use FenderBinaryOperator::*;
    match op {
        "+" => Add,
        "-" => Sub,
        "*" => Mul,
        "/" => Div,
        "&&" => And,
        "||" => Or,
        ">" => Gt,
        "<" => Lt,
        "<=" => Le,
        ">=" => Ge,
        "==" => Eq,
        "!=" => Ne,
        "%" => Mod,
        op_str => unreachable!("{op_str}"),
    }
}

pub(crate) fn parse_binary_operation(
    token: &Token,
    engine: &mut ExecutionEngine<FenderTypeSystem>,
    scope: &mut LexicalScope,
) -> InterpreterResult {
    let mut left = parse_expr(&token.children[0], engine, scope)?;
    let mut last_end = token.children[0].range.end;
    for right in &token.children[1..] {
        let op: String = right.source[last_end..right.range.start].iter().collect();
        let op = parse_binary_operator(op.trim());
        last_end = right.range.end;
        let right = parse_expr(right, engine, scope)?;
        left = Expression::BinaryOpEval(op, [left, right].into());
    }
    Ok(left)
}

pub(crate) fn parse_value(
    token: &Token,
    engine: &mut ExecutionEngine<FenderTypeSystem>,
    scope: &mut LexicalScope,
) -> InterpreterResult {
    Ok(match token.children[0].get_name().as_deref().unwrap() {
        "literal" => parse_literal(&token.children[0], engine, scope)?,
        "lambdaParameter" => Expression::stack(0),
        "enclosedExpr" => parse_expr(&token.children[0], engine, scope)?,
        "name" => {
            let name = token.get_match();
            let found = scope.resolve_propagate(engine, &name, token.range.start)?;
            match found {
                VariableType::Captured(addr) => Expression::captured(addr),
                VariableType::Stack(addr) => Expression::stack(addr),
                VariableType::Global(addr) => Expression::global(addr),
            }
        }
        name => unreachable!("{name}"),
    })
}

pub(crate) fn parse_invoke_args(
    token: &Token,
    engine: &mut ExecutionEngine<FenderTypeSystem>,
    scope: &mut LexicalScope,
) -> FenderResult<Vec<Expression<FenderTypeSystem>>> {
    let token = &token.children[0];
    match token.get_name().as_deref().unwrap() {
        "invokeArgs" => token
            .children_named("expr")
            .map(|arg| parse_expr(arg, engine, scope))
            .collect(),
        "codeBody" => Ok(vec![parse_closure(token, engine, scope)?]),
        name => unreachable!("{name}"),
    }
}

pub(crate) fn parse_tail_operation(
    token: &Token,
    expr: Expression<FenderTypeSystem>,
    engine: &mut ExecutionEngine<FenderTypeSystem>,
    scope: &mut LexicalScope,
) -> InterpreterResult {
    let token = &token.children[0];
    match token.get_name().as_deref().unwrap() {
        "invoke" => {
            let args = parse_invoke_args(token, engine, scope)?;
            Ok(Expression::DynamicFunctionCall(expr.into(), args))
        }
        "receiverCall" => {
            let name = token.children[0].get_match();
            let function = match scope.resolve_propagate(engine, &name, token.range.start)? {
                VariableType::Captured(addr) => Expression::captured(addr),
                VariableType::Stack(addr) => Expression::stack(addr),
                VariableType::Global(addr) => Expression::global(addr),
            };
            let mut args = parse_invoke_args(&token.children[1], engine, scope)?;
            args.insert(0, expr);
            Ok(Expression::DynamicFunctionCall(function.into(), args))
        }
        "index" => {
            let pos = parse_expr(&token.children[0], engine, scope)?;
            Ok(Expression::BinaryOpEval(
                FenderBinaryOperator::Index,
                [expr, pos].into(),
            ))
        }
        "fieldAccess" => {
            let field_id = engine
                .context
                .struct_table
                .field_index(&token.children[0].get_match());
            Ok(Expression::BinaryOpEval(
                FenderBinaryOperator::FieldAccess,
                [
                    expr,
                    Expression::RawValue(FenderValue::Int(field_id as i64).into()),
                ]
                .into(),
            ))
        }
        name => unreachable!("{name}"),
    }
}

pub(crate) fn parse_term(
    token: &Token,
    engine: &mut ExecutionEngine<FenderTypeSystem>,
    scope: &mut LexicalScope,
) -> InterpreterResult {
    let value = token.children_named("value").next().unwrap();
    let mut value = parse_value(value, engine, scope)?;
    if let Some("tailOperationChain") = token.children[token.children.len() - 1]
        .get_name()
        .as_deref()
    {
        let tail_operation_chain = &token.children[token.children.len() - 1];
        for tail_operation in &tail_operation_chain.children {
            value = parse_tail_operation(tail_operation, value, engine, scope)?;
        }
    }
    if let Some("unaryOperator") = token.children[0].get_name().as_deref() {
        let op = match &*token.children[0].get_match() {
            "-" => FenderUnaryOperator::Neg,
            "!" => FenderUnaryOperator::BoolNeg,
            name => unreachable!("{name}"),
        };
        value = Expression::UnaryOpEval(op, value.into());
    }
    Ok(value)
}

pub(crate) fn parse_literal(
    token: &Token,
    engine: &mut ExecutionEngine<FenderTypeSystem>,
    scope: &mut LexicalScope,
) -> InterpreterResult {
    let token = &token.children[0];
    Ok(match token.get_name().as_deref().unwrap() {
        "int" => FenderValue::Int(unwrap_rust!(token.get_match().parse())?).into(),
        "float" => FenderValue::Float(unwrap_rust!(token.get_match().parse())?).into(),
        "boolean" => FenderValue::Bool(unwrap_rust!(token.get_match().parse())?).into(),
        "string" => parse_string(token, engine, scope)?,
        "char" => unwrap_rust!(parse_char(token))?.into(),
        "list" => parse_list(token, engine, scope)?,
        "null" => FenderValue::Null.into(),
        "closure" => parse_closure(token, engine, scope)?,
        "structInit" => parse_struct_instantiation(token, engine, scope)?,
        "hashMap" => parse_hash_map(token, engine, scope)?,
        name => unreachable!("{name}"),
    })
}

fn parse_hash_map(
    token: &Token,
    engine: &mut ExecutionEngine<FenderTypeSystem>,
    scope: &mut LexicalScope,
) -> InterpreterResult {
    let mut values = Vec::new();
    for entry in token.children_named("mapEntry") {
        for value in &entry.children {
            values.push(parse_expr(value, engine, scope)?)
        }
    }
    Ok(Expression::Initialize(FenderInitializer::HashMap, values))
}

fn parse_struct_instantiation(
    token: &Token,
    engine: &mut ExecutionEngine<FenderTypeSystem>,
    scope: &mut LexicalScope,
) -> InterpreterResult {
    let mut name = String::new();
    let mut fields = HashMap::new();
    for sub in token.children.iter() {
        match sub.get_name().as_deref().unwrap() {
            "name" => {
                if !engine
                    .context
                    .struct_table
                    .struct_name_index()
                    .contains_key(&sub.get_match())
                {
                    return Err(InterpreterError::UnresolvedName(
                        sub.get_match(),
                        token.range.start,
                    )
                    .into());
                }
                name = sub.get_match()
            }
            "structEntry" => {
                fields.insert(sub.children[0].get_match(), &sub.children[1]);
            }
            e => unreachable!("{:?}", e),
        }
    }

    let mut values = Vec::new();

    let id = engine.context.struct_table.struct_name_index()[&name];
    for f_name in engine.context.struct_table.type_list()[id]
        .fields
        .iter()
        .map(|v| v.0.clone())
        .collect::<Vec<_>>()
    {
        match fields.get(&f_name) {
            Some(t) => values.push(parse_expr(t, engine, scope)?),
            None => values.push(FenderValue::Null.into()),
        }
    }

    Ok(Expression::Initialize(
        FenderInitializer::Struct(id),
        values,
    ))
}

fn parse_list(
    token: &Token,
    engine: &mut ExecutionEngine<FenderTypeSystem>,
    scope: &mut LexicalScope,
) -> InterpreterResult {
    let mut values = Vec::new();
    for child in &token.children {
        values.push(parse_expr(child, engine, scope)?);
    }
    Ok(Expression::Initialize(FenderInitializer::List, values))
}

pub(crate) fn parse_string(
    token: &Token,
    engine: &mut ExecutionEngine<FenderTypeSystem>,
    scope: &mut LexicalScope,
) -> FenderResult<Expression<FenderTypeSystem>> {
    let mut exprs = Vec::new();
    let mut str = String::new();
    for child in &token.children {
        match child.get_name().as_deref().unwrap() {
            "strChar" => str.push(child.source[child.range.start]),
            "escapeSequence" => str.push(parse_escape_seq(child)?),
            "strExpr" => {
                exprs.push(Expression::from(FenderValue::String(str.into())));
                str = String::new();
                exprs.push(parse_expr(&child.children[0], engine, scope)?);
            }
            name => unreachable!("{name}"),
        }
    }
    if exprs.is_empty() {
        return Ok(FenderValue::String(str.into()).into());
    }
    exprs.push(FenderValue::String(str.into()).into());
    Ok(Expression::Initialize(FenderInitializer::String, exprs))
}

pub(crate) fn parse_char(token: &Token) -> Result<FenderValue, Box<dyn Error>> {
    assert!(token.children.len() == 1);
    Ok(FenderValue::Char(
        match token.children[0].get_name().as_deref().unwrap() {
            "escapeSequence" => parse_escape_seq(&token.children[0])?,
            "innerChar" => token.children[0].get_match().chars().next().unwrap(),
            name => unreachable!("{name:?}"),
        },
    ))
}

fn parse_escape_seq(token: &Token) -> FenderResult<char> {
    let escape: Vec<_> = token.get_match().bytes().collect();
    Ok(match escape[1] as char {
        'n' => '\n',
        'r' => '\r',
        '"' => '"',
        't' => '\t',
        'u' => {
            let code = String::from_utf8_lossy(&escape[2..]);
            unsafe { char::from_u32_unchecked(unwrap_rust!(u32::from_str_radix(&code, 16))?) }
        }
        _ => escape[1] as char,
    })
}

pub(crate) fn parse_expr(
    token: &Token,
    engine: &mut ExecutionEngine<FenderTypeSystem>,
    scope: &mut LexicalScope,
) -> InterpreterResult {
    Ok(match token.get_name().as_deref().unwrap() {
        "add" | "mul" | "pow" | "range" | "cmp" | "or" | "and" => {
            parse_binary_operation(token, engine, scope)?
        }
        "term" => parse_term(token, engine, scope)?,
        "expr" | "enclosedExpr" => {
            let label = token.children_named("label").next().map(|t| t.get_match());
            let mut expr = parse_expr(&token.children[label.iter().count()], engine, scope)?;
            if let Some(label) = label {
                let return_target = scope
                    .labels
                    .get(&label)
                    .ok_or(InterpreterError::UnresolvedLabel(label, token.range.start))?;
                expr = Expression::ReturnTarget(*return_target, expr.into());
            }
            expr
        }
        name => unreachable!("{name}"),
    })
}
