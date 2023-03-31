use self::error::InterpreterError;
use crate::{
    fender_value::{fender_structs::FenderStructType, FenderValue},
    lazy_cell::LazyCell,
    operators::FenderBinaryOperator,
    operators::FenderInitializer,
    operators::FenderUnaryOperator,
    stdlib,
    type_sys::{
        type_id::FenderTypeId,
        type_system::{FenderGlobalContext, FenderTypeSystem},
    },
};
use flux_bnf::{
    lexer::{CullStrategy, Lexer},
    tokens::{
        iterators::{IgnoreTokens, SelectTokens},
        Token,
    },
};
use freight_vm::{
    execution_engine::ExecutionEngine,
    expression::{Expression, VariableType},
    function::{ArgCount, FunctionRef, FunctionType, FunctionWriter},
    vm_writer::VMWriter,
};
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    error::Error,
    process::exit,
    rc::Rc,
};

pub mod error;

pub type InterpreterResult = Result<Expression<FenderTypeSystem>, Box<dyn Error>>;

pub struct LexicalScope<'a> {
    globals: Rc<HashMap<String, usize>>,
    labels: Rc<HashMap<String, usize>>,
    args: ArgCount,
    captures: RefCell<Vec<VariableType>>,
    variables: RefCell<HashMap<String, VariableType>>,
    parent: Option<&'a LexicalScope<'a>>,
    return_target: usize,
}

impl<'a> LexicalScope<'a> {
    pub fn child_scope(&self, args: ArgCount, return_target: usize) -> LexicalScope {
        LexicalScope {
            globals: self.globals.clone(),
            labels: self.labels.clone(),
            args,
            captures: vec![].into(),
            variables: HashMap::new().into(),
            parent: Some(self),
            return_target,
        }
    }

    pub fn capture(&self, name: &str, src_pos: usize) -> Result<(), InterpreterError> {
        let parent_var = self
            .parent
            .and_then(|parent| parent.variables.borrow().get(name).cloned())
            .ok_or_else(|| InterpreterError::UnresolvedName(name.to_string(), src_pos))?;
        let mut captures = self.captures.borrow_mut();
        captures.push(parent_var);
        self.variables
            .borrow_mut()
            .insert(name.to_string(), VariableType::Captured(captures.len() - 1));
        Ok(())
    }

    pub fn resolve_propagate(
        &self,
        name: &str,
        src_pos: usize,
    ) -> Result<VariableType, InterpreterError> {
        let mut parent_scopes = vec![];
        let mut cur = self;
        while !cur.variables.borrow().contains_key(name) {
            parent_scopes.push(cur);
            match &cur.parent {
                Some(parent) => cur = parent,
                None => {
                    if let Some(var) = self.globals.get(name) {
                        return Ok(VariableType::Global(*var));
                    } else {
                        return Err(InterpreterError::UnresolvedName(name.to_string(), src_pos));
                    }
                }
            }
        }
        for scope in parent_scopes.into_iter().rev() {
            scope.capture(name, src_pos)?;
        }
        Ok(self.variables.borrow()[name].clone())
    }

    pub fn top_level_return(&self) -> usize {
        if self.parent.is_none() {
            return 0;
        }
        let mut scope = self;
        while scope.parent.and_then(|p| p.parent).is_some() {
            scope = scope.parent.expect("Has parent");
        }
        scope.return_target
    }
}

static LEXER: LazyCell<Lexer> = LazyCell::new(|| {
    let mut lex = match flux_bnf::bnf::parse(include_str!("../../fender.bnf")) {
        Ok(lex) => lex,
        Err(e) => {
            eprintln!("Invalid BNF: {e}");
            exit(1);
        }
    };
    lex.set_unnamed_rule(CullStrategy::LiftChildren);
    lex.add_rule_for_names(
        vec![
            "sep",
            "lineSep",
            "lineBreak",
            "comment",
            "cmpOp",
            "newLine",
            "break",
            "alpha",
            "alphanum",
        ],
        CullStrategy::DeleteAll,
    );
    lex.add_rule_for_names(
        vec!["or", "and", "pow", "mul", "add", "cmp", "range"],
        CullStrategy::LiftAtMost(1),
    );
    lex
});

pub fn create_vm(source: &str) -> Result<ExecutionEngine<FenderTypeSystem>, Box<dyn Error>> {
    let lex_read = LEXER.get();
    let lex = lex_read.as_ref().unwrap();
    let root = lex.tokenize(source)?;
    parse_main_function(&root)
}

fn parse_main_function(token: &Token) -> Result<ExecutionEngine<FenderTypeSystem>, Box<dyn Error>> {
    let mut vm = VMWriter::new();
    let mut main = FunctionWriter::new(ArgCount::Fixed(0));
    let main_return_target = vm.create_return_target();
    let mut globals = HashMap::new();
    for t in token.children.iter() {
        let child = &t.children[0];
        if let Some("declaration") = child.get_name().as_deref() {
            let name = child.children[0].get_match();
            globals.insert(name, vm.create_global());
        }
    }
    let labels = token
        .rec_iter()
        .select_token("label")
        .map(|t| t.children[0].get_match())
        .collect::<HashSet<_>>()
        .into_iter()
        .enumerate()
        .map(|(i, name)| (name, i))
        .collect();

    let mut global_context = FenderGlobalContext {
        deps: stdlib::detect_load(token, &mut globals, &mut main, &mut vm),
        struct_table: Default::default(),
    };

    let mut scope = LexicalScope {
        globals: Rc::new(globals),
        labels: Rc::new(labels),
        args: ArgCount::Fixed(0),
        captures: Default::default(),
        variables: Default::default(),
        parent: None,
        return_target: main_return_target,
    };
    for statement in &token.children {
        let statement = parse_statement(
            statement,
            &mut vm,
            &mut scope,
            &mut main,
            &mut global_context,
            true,
        )?;
        main.evaluate_expression(statement);
    }
    let main_ref = vm.include_function(main, main_return_target);
    Ok(vm.finish(main_ref, global_context))
}

fn code_body_uses_lambda_parameter(token: &Token) -> bool {
    token
        .rec_iter()
        .ignore_token("codeBody")
        .select_token("lambdaParameter")
        .next()
        .is_some()
}

fn parse_args(token: &Token) -> (Vec<String>, Vec<String>, Option<String>) {
    let mut arg_names = vec![];
    let mut optional_arg_names = vec![];
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

fn parse_closure(
    token: &Token,
    writer: &mut VMWriter<FenderTypeSystem>,
    scope: &mut LexicalScope,
    global_context: &mut FenderGlobalContext,
) -> Result<Expression<FenderTypeSystem>, Box<dyn Error>> {
    let function_ref = match token.matcher_name.as_deref().unwrap() {
        "closure" if token.children.len() == 2 => {
            let args = &token.children[0];
            let code_body = &token.children[1];

            let (args, optional_args, varg_name) = parse_args(args);

            let arg_count = match (optional_args.len(), &varg_name) {
                (op_arg_count, Some(_)) => ArgCount::Variadic {
                    min: args.len(),
                    max: op_arg_count + op_arg_count,
                },
                (op_arg_count, None) if op_arg_count > 0 => ArgCount::Range {
                    min: args.len(),
                    max: args.len() + op_arg_count,
                },
                _ => ArgCount::Fixed(args.len()),
            };

            let mut new_scope = scope.child_scope(arg_count, writer.create_return_target());
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
            parse_code_body(code_body, writer, &mut new_scope, global_context)
        }
        "closure" | "codeBody" => {
            let code_body = token
                .matcher_name
                .as_deref()
                .map(|n| n == "codeBody")
                .and_then(|b| b.then_some(token))
                .unwrap_or_else(|| &token.children[0]);
            let arg_count = code_body_uses_lambda_parameter(code_body) as usize;
            let mut new_scope =
                scope.child_scope(ArgCount::Fixed(arg_count), writer.create_return_target());
            parse_code_body(code_body, writer, &mut new_scope, global_context)
        }
        _ => unreachable!(),
    }?;
    Ok(match &function_ref.function_type {
        FunctionType::CapturingDef(_) => Expression::FunctionCapture(function_ref),
        _ => FenderValue::Function(function_ref).into(),
    })
}

fn parse_code_body(
    token: &Token,
    writer: &mut VMWriter<FenderTypeSystem>,
    new_scope: &mut LexicalScope,
    global_context: &mut FenderGlobalContext,
) -> Result<FunctionRef<FenderTypeSystem>, Box<dyn Error>> {
    let mut function = FunctionWriter::new(new_scope.args);
    for statement in &token.children {
        let expr = parse_statement(
            statement,
            writer,
            new_scope,
            &mut function,
            global_context,
            false,
        )?;
        function.evaluate_expression(expr);
    }
    let captures = std::mem::take(&mut *new_scope.captures.borrow_mut());
    if !captures.is_empty() {
        function.set_captures(captures);
    }
    Ok(writer.include_function(function, new_scope.return_target))
}

fn parse_statement(
    token: &Token,
    writer: &mut VMWriter<FenderTypeSystem>,
    scope: &mut LexicalScope,
    function: &mut FunctionWriter<FenderTypeSystem>,
    global_context: &mut FenderGlobalContext,
    use_globals: bool,
) -> InterpreterResult {
    let token = &token.children[0];
    Ok(match token.get_name().as_deref().unwrap() {
        "expr" => parse_expr(token, writer, scope, global_context)?,
        "assignment" => parse_assignment(token, writer, scope, global_context)?,
        "return" => parse_return(token, writer, scope, global_context)?,
        "declaration" if use_globals => {
            let name = token.children[0].get_match();
            let var = scope.globals.get(&name).copied().ok_or_else(|| {
                InterpreterError::UnresolvedName(name.to_string(), token.range.start)
            })?;
            let expr = &token.children[1];
            let expr = parse_expr(expr, writer, scope, global_context)?;
            Expression::AssignGlobal(var, expr.into())
        }
        "declaration" if !use_globals => {
            let name = token.children[0].get_match();
            let variables = scope.variables.borrow();
            let existing = variables.get(&name);
            if matches!(
                existing,
                Some(VariableType::Stack(_) | VariableType::Captured(_))
            ) {
                return Err(
                    InterpreterError::DuplicateName(name.to_string(), token.range.start).into(),
                );
            }
            drop(variables);
            let var = function.create_variable();
            let expr = &token.children[1];
            let expr = parse_expr(expr, writer, scope, global_context)?;
            scope
                .variables
                .borrow_mut()
                .insert(name.to_string(), VariableType::Stack(var));
            Expression::AssignStack(var, expr.into())
        }
        "structDeclaration" => {
            parse_struct_declaration(token, writer, scope, global_context, function)?
        }
        name => unreachable!("{name}"),
    })
}

fn parse_struct_declaration(
    token: &Token,
    writer: &mut VMWriter<FenderTypeSystem>,
    scope: &mut LexicalScope,
    global_context: &mut FenderGlobalContext,
    outer_function: &mut FunctionWriter<FenderTypeSystem>,
) -> InterpreterResult {
    let mut struct_name = String::new();
    let mut fields = Vec::new();

    for child_token in token.iter() {
        match child_token.get_name().as_deref().unwrap() {
            "name" => struct_name = child_token.get_match(),
            "structBody" => {
                for arg_token in child_token.iter() {
                    fields.push((
                        arg_token.children[0].get_match(),
                        if arg_token.children.len() > 1 {
                            FenderTypeId::type_from_str(
                                arg_token.children[1].children[0].get_match(),
                            )
                        } else {
                            None
                        },
                    ));
                }
            }
            e => unreachable!("{}", e),
        }
    }
    let new_scope = scope.child_scope(ArgCount::Fixed(fields.len()), writer.create_return_target());
    let mut constructor = FunctionWriter::new(ArgCount::Fixed(fields.len()));
    let mut exprs = Vec::with_capacity(fields.len());
    for i in 0..fields.len() {
        exprs.push(Expression::Variable(VariableType::Stack(i)));
    }
    constructor.evaluate_expression(Expression::Initialize(
        FenderInitializer::Struct(global_context.struct_table.len()),
        exprs,
    ));

    // constructor.evaluate_expression(expr)

    global_context.struct_table.push(Rc::new(FenderStructType {
        name: struct_name.clone(),
        fields,
    }));
    // let t = writer.include_function(constructor, new_scope.return_target);
    // Ok(Expression::AssignDynamic([, writer.include_function(constructor, new_scope.return_target)].into()))

    let constructor_var = outer_function.create_variable();
    scope
        .variables
        .borrow_mut()
        .insert(struct_name, VariableType::Stack(constructor_var));

    Ok(Expression::AssignStack(
        constructor_var,
        Expression::from(FenderValue::Function(
            writer.include_function(constructor, new_scope.return_target),
        ))
        .into(),
    ))

    // Ok(FenderValue::Function(writer.include_function(constructor, new_scope.return_target)).into())
}

fn parse_assignment(
    token: &Token,
    writer: &mut VMWriter<FenderTypeSystem>,
    scope: &mut LexicalScope,
    global_context: &mut FenderGlobalContext,
) -> InterpreterResult {
    dbg!("here");

    let target = &token.children[0];
    let value = &token.children[token.children.len() - 1];
    let op = token
        .children_named("assignOp")
        .next()
        .map(|t| parse_binary_operator(&t.get_match()));
    let target = parse_expr(target, writer, scope, global_context)?;
    dbg!(&target);
    let value = parse_expr(value, writer, scope, global_context)?;
    if let Some(_op) = op {
        todo!()
    }
    Ok(Expression::AssignDynamic([target, value].into()))
}

fn parse_return(
    token: &Token,
    writer: &mut VMWriter<FenderTypeSystem>,
    scope: &mut LexicalScope,
    global_context: &mut FenderGlobalContext,
) -> InterpreterResult {
    let name = token.children_named("name").next().map(|t| t.get_match());
    let expr = token.children_named("expr").next();
    let expr = if let Some(expr) = expr {
        parse_expr(expr, writer, scope, global_context)?
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

fn parse_binary_operator(op: &str) -> FenderBinaryOperator {
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

fn parse_binary_operation(
    token: &Token,
    writer: &mut VMWriter<FenderTypeSystem>,
    scope: &mut LexicalScope,
    global_context: &mut FenderGlobalContext,
) -> InterpreterResult {
    let mut left = parse_expr(&token.children[0], writer, scope, global_context)?;
    let mut last_end = token.children[0].range.end;
    for right in &token.children[1..] {
        let op: String = right.source[last_end..right.range.start].iter().collect();
        let op = parse_binary_operator(op.trim());
        last_end = right.range.end;
        let right = parse_expr(right, writer, scope, global_context)?;
        left = Expression::BinaryOpEval(op, [left, right].into());
    }
    Ok(left)
}

fn parse_value(
    token: &Token,
    writer: &mut VMWriter<FenderTypeSystem>,
    scope: &mut LexicalScope,
    global_context: &mut FenderGlobalContext,
) -> InterpreterResult {
    Ok(match token.children[0].get_name().as_deref().unwrap() {
        "literal" => parse_literal(&token.children[0], writer, scope, global_context)?,
        "lambdaParameter" => Expression::stack(0),
        "enclosedExpr" => parse_expr(&token.children[0], writer, scope, global_context)?,
        "name" => match scope.resolve_propagate(&token.get_match(), token.range.start)? {
            VariableType::Captured(addr) => Expression::captured(addr),
            VariableType::Stack(addr) => Expression::stack(addr),
            VariableType::Global(addr) => Expression::global(addr),
        },
        name => unreachable!("{name}"),
    })
}

fn parse_invoke_args(
    token: &Token,
    writer: &mut VMWriter<FenderTypeSystem>,
    scope: &mut LexicalScope,
    global_context: &mut FenderGlobalContext,
) -> Result<Vec<Expression<FenderTypeSystem>>, Box<dyn Error>> {
    let token = &token.children[0];
    match token.get_name().as_deref().unwrap() {
        "invokeArgs" => token
            .children_named("expr")
            .map(|arg| parse_expr(arg, writer, scope, global_context))
            .collect(),
        "codeBody" => Ok(vec![parse_closure(token, writer, scope, global_context)?]),
        name => unreachable!("{name}"),
    }
}

fn parse_tail_operation(
    token: &Token,
    expr: Expression<FenderTypeSystem>,
    writer: &mut VMWriter<FenderTypeSystem>,
    scope: &mut LexicalScope,
    global_context: &mut FenderGlobalContext,
) -> InterpreterResult {
    let token = &token.children[0];
    match token.get_name().as_deref().unwrap() {
        "invoke" => {
            let args = parse_invoke_args(token, writer, scope, global_context)?;
            Ok(Expression::DynamicFunctionCall(expr.into(), args))
        }
        "receiverCall" => {
            let name = token.children[0].get_match();
            let function = match scope.resolve_propagate(&name, token.range.start)? {
                VariableType::Captured(addr) => Expression::captured(addr),
                VariableType::Stack(addr) => Expression::stack(addr),
                VariableType::Global(addr) => Expression::global(addr),
            };
            let mut args = parse_invoke_args(&token.children[1], writer, scope, global_context)?;
            args.insert(0, expr);
            Ok(Expression::DynamicFunctionCall(function.into(), args))
        }
        "index" => {
            let pos = parse_expr(&token.children[0], writer, scope, global_context)?;
            Ok(Expression::BinaryOpEval(
                FenderBinaryOperator::Index,
                [expr, pos].into(),
            ))
        }
        name => unreachable!("{name}"),
    }
}

fn parse_term(
    token: &Token,
    writer: &mut VMWriter<FenderTypeSystem>,
    scope: &mut LexicalScope,
    global_context: &mut FenderGlobalContext,
) -> InterpreterResult {
    let value = token.children_named("value").next().unwrap();
    let mut value = parse_value(value, writer, scope, global_context)?;
    if let Some("tailOperationChain") = token.children[token.children.len() - 1]
        .get_name()
        .as_deref()
    {
        let tail_operation_chain = &token.children[token.children.len() - 1];
        for tail_operation in &tail_operation_chain.children {
            value = parse_tail_operation(tail_operation, value, writer, scope, global_context)?;
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

fn parse_literal(
    token: &Token,
    writer: &mut VMWriter<FenderTypeSystem>,
    scope: &mut LexicalScope,
    global_context: &mut FenderGlobalContext,
) -> InterpreterResult {
    let token = &token.children[0];
    Ok(match token.get_name().as_deref().unwrap() {
        "int" => FenderValue::Int(token.get_match().parse()?).into(),
        "float" => FenderValue::Float(token.get_match().parse()?).into(),
        "boolean" => FenderValue::Bool(token.get_match().parse()?).into(),
        "string" => parse_string(token, writer, scope, global_context)?,
        "char" => parse_char(token)?.into(),
        "list" => parse_list(token, writer, scope, global_context)?,
        "null" => FenderValue::Null.into(),
        "closure" => parse_closure(token, writer, scope, global_context)?,
        name => unreachable!("{name}"),
    })
}

fn parse_list(
    token: &Token,
    writer: &mut VMWriter<FenderTypeSystem>,
    scope: &mut LexicalScope,
    global_context: &mut FenderGlobalContext,
) -> InterpreterResult {
    let mut values = vec![];
    for child in &token.children {
        values.push(parse_expr(child, writer, scope, global_context)?);
    }
    Ok(Expression::Initialize(FenderInitializer::List, values))
}

fn parse_string(
    token: &Token,
    writer: &mut VMWriter<FenderTypeSystem>,
    scope: &mut LexicalScope,
    global_context: &mut FenderGlobalContext,
) -> Result<Expression<FenderTypeSystem>, Box<dyn Error>> {
    let mut exprs = vec![];
    let mut str = String::new();
    for child in &token.children {
        match child.get_name().as_deref().unwrap() {
            "strChar" => str.push(child.source[child.range.start]),
            "escapeSequence" => str.push(parse_escape_seq(child)?),
            "strExpr" => {
                exprs.push(Expression::from(FenderValue::String(str.into())));
                str = String::new();
                exprs.push(parse_expr(
                    &child.children[0],
                    writer,
                    scope,
                    global_context,
                )?);
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

fn parse_char(token: &Token) -> Result<FenderValue, Box<dyn Error>> {
    assert!(token.children.len() == 1);
    Ok(FenderValue::Char(
        match token.children[0].get_name().as_deref().unwrap() {
            "escapeSequence" => parse_escape_seq(&token.children[0])?,
            "innerChar" => token.children[0].get_match().chars().next().unwrap(),
            name => unreachable!("{name:?}"),
        },
    ))
}

fn parse_escape_seq(token: &Token) -> Result<char, Box<dyn Error>> {
    let escape: Vec<_> = token.get_match().bytes().collect();
    Ok(match escape[1] as char {
        'n' => '\n',
        'r' => '\r',
        '"' => '"',
        't' => '\t',
        'u' => {
            let code = String::from_utf8_lossy(&escape[2..]);
            unsafe { char::from_u32_unchecked(u32::from_str_radix(&code, 16)?) }
        }
        _ => escape[1] as char,
    })
}

fn parse_expr(
    token: &Token,
    writer: &mut VMWriter<FenderTypeSystem>,
    scope: &mut LexicalScope,
    global_context: &mut FenderGlobalContext,
) -> InterpreterResult {
    Ok(match token.get_name().as_deref().unwrap() {
        "add" | "mul" | "pow" | "range" | "cmp" | "or" | "and" => {
            parse_binary_operation(token, writer, scope, global_context)?
        }
        "term" => parse_term(token, writer, scope, global_context)?,
        "expr" | "enclosedExpr" => {
            let label = token.children_named("label").next().map(|t| t.get_match());
            let mut expr = parse_expr(
                &token.children[label.iter().count()],
                writer,
                scope,
                global_context,
            )?;
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
