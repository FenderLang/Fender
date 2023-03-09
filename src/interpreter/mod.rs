pub mod error;

use crate::lazy_cell::LazyCell;
use crate::operators::FenderInitializer;
use crate::stdlib;
use crate::{
    operators::FenderBinaryOperator, operators::FenderUnaryOperator, FenderTypeSystem, FenderValue,
};
use flux_bnf::lexer::{CullStrategy, Lexer};
use flux_bnf::tokens::iterators::ignore_iter::IgnoreTokensIteratorExt;
use flux_bnf::tokens::Token;
use freight_vm::execution_engine::ExecutionEngine;
use freight_vm::expression::{Expression, VariableType};
use freight_vm::function::{FunctionRef, FunctionType, FunctionWriter};
use freight_vm::vm_writer::VMWriter;
use std::cell::RefCell;
use std::collections::HashMap;
use std::error::Error;
use std::rc::Rc;

use self::error::InterpreterError;

pub struct LexicalScope<'a> {
    globals: Rc<HashMap<String, usize>>,
    args: usize,
    captures: RefCell<Vec<VariableType>>,
    variables: RefCell<HashMap<String, VariableType>>,
    parent: Option<Rc<&'a LexicalScope<'a>>>,
}

impl<'a> LexicalScope<'a> {
    pub fn child_scope(&self, args: usize) -> LexicalScope {
        LexicalScope {
            globals: self.globals.clone(),
            args,
            captures: vec![].into(),
            variables: HashMap::new().into(),
            parent: Some(Rc::new(self)),
        }
    }

    pub fn capture(&self, name: &str) -> Result<(), InterpreterError> {
        let parent_var = self
            .parent
            .as_ref()
            .and_then(|parent| parent.variables.borrow().get(name).cloned())
            .ok_or_else(|| InterpreterError::UnresolvedName(name.to_string()))?;
        let mut captures = self.captures.borrow_mut();
        captures.push(parent_var);
        self.variables
            .borrow_mut()
            .insert(name.to_string(), VariableType::Captured(captures.len() - 1));
        Ok(())
    }

    pub fn resolve_propagate(&self, name: &str) -> Result<VariableType, InterpreterError> {
        if let Some(var) = self.globals.get(name) {
            return Ok(VariableType::Global(*var));
        }
        let mut parent_scopes = vec![];
        let mut cur = self;
        while !cur.variables.borrow().contains_key(name) {
            parent_scopes.push(cur);
            match &cur.parent {
                Some(parent) => cur = parent,
                None => return Err(InterpreterError::UnresolvedName(name.to_string())),
            }
        }
        for scope in parent_scopes.into_iter().rev() {
            scope.capture(name)?;
        }
        Ok(self.variables.borrow()[name].clone())
    }
}

static LEXER: LazyCell<Lexer> = LazyCell::new(|| {
    let mut lex = flux_bnf::bnf::parse(include_str!("../../fender.bnf")).expect("Invalid BNF");
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
    let mut main = FunctionWriter::new(0);
    let mut globals = HashMap::new();
    for t in token.children.iter() {
        let child = &t.children[0];
        if let Some("declaration") = child.get_name().as_deref() {
            let name = child.children[0].get_match();
            globals.insert(name, vm.create_global());
        }
    }
    stdlib::loader::detect_load(token, &mut globals, &mut main, &mut vm);
    let mut scope = LexicalScope {
        globals: Rc::new(globals),
        args: 0,
        captures: Default::default(),
        variables: Default::default(),
        parent: None,
    };
    for statement in &token.children {
        let statement = parse_statement(statement, &mut vm, &mut scope, &mut main, true)?;
        main.evaluate_expression(statement);
    }
    let main_ref = vm.include_function(main);
    Ok(vm.finish(main_ref))
}

fn code_body_uses_lambda_parameter(token: &Token) -> bool {
    token
        .recursive_children_named("lambdaParameter")
        .ignore_token("codeBody")
        .next()
        .is_some()
}

fn parse_args(token: &Token) -> Vec<String> {
    let mut arg_names = vec![];
    for arg in &token.children {
        if arg.children.len() == 2 {
            unimplemented!();
        }
        let name = arg.children[0].get_match();
        arg_names.push(name);
    }
    arg_names
}

fn parse_closure(
    token: &Token,
    writer: &mut VMWriter<FenderTypeSystem>,
    scope: &mut LexicalScope,
) -> Result<FunctionRef<FenderTypeSystem>, Box<dyn Error>> {
    match token.children.len() {
        1 => {
            let code_body = &token.children[0];
            let arg_count = if code_body_uses_lambda_parameter(code_body) {
                1
            } else {
                0
            };
            let mut new_scope = scope.child_scope(arg_count);
            parse_code_body(code_body, writer, &mut new_scope)
        }
        2 => {
            let args = &token.children[0];
            let code_body = &token.children[1];
            let args = parse_args(args);
            let mut new_scope = scope.child_scope(args.len());
            for (index, arg) in args.into_iter().enumerate() {
                new_scope
                    .variables
                    .borrow_mut()
                    .insert(arg, VariableType::Stack(index));
            }
            parse_code_body(code_body, writer, &mut new_scope)
        }
        _ => unreachable!(),
    }
}

fn parse_code_body(
    token: &Token,
    writer: &mut VMWriter<FenderTypeSystem>,
    new_scope: &mut LexicalScope,
) -> Result<FunctionRef<FenderTypeSystem>, Box<dyn Error>> {
    let mut function = FunctionWriter::new(new_scope.args);
    for statement in &token.children {
        let expr = parse_statement(statement, writer, new_scope, &mut function, false)?;
        function.evaluate_expression(expr);
    }
    let captures = std::mem::take(&mut *new_scope.captures.borrow_mut());
    if captures.len() > 0 {
        function.set_captures(captures);
    }
    Ok(writer.include_function(function))
}

fn parse_statement(
    token: &Token,
    writer: &mut VMWriter<FenderTypeSystem>,
    scope: &mut LexicalScope,
    function: &mut FunctionWriter<FenderTypeSystem>,
    use_globals: bool,
) -> Result<Expression<FenderTypeSystem>, Box<dyn Error>> {
    let token = &token.children[0];
    Ok(match token.get_name().as_deref().unwrap() {
        "expr" => parse_expr(token, writer, scope)?,
        "assignment" => {
            let target = &token.children[0];
            let value = &token.children[token.children.len() - 1];
            if token.children_named("assignOp").count() > 0 {
                unimplemented!()
            }
            let target = parse_expr(target, writer, scope)?;
            let value = parse_expr(value, writer, scope)?;
            Expression::AssignDynamic([target, value].into())
        }
        "declaration" if use_globals => {
            let name = token.children[0].get_match();
            let var = scope
                .globals
                .get(&name)
                .copied()
                .ok_or_else(|| InterpreterError::UnresolvedName(name.to_string()))?;
            let expr = &token.children[1];
            let expr = parse_expr(expr, writer, scope)?;
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
                return Err(InterpreterError::DuplicateName(name.to_string()).into());
            }
            drop(variables);
            let var = function.create_variable();
            let expr = &token.children[1];
            let expr = parse_expr(expr, writer, scope)?;
            scope
                .variables
                .borrow_mut()
                .insert(name.to_string(), VariableType::Stack(var));
            Expression::AssignStack(var, expr.into())
        }
        _ => unreachable!(),
    })
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
        _ => unreachable!(),
    }
}

fn parse_binary_operation(
    token: &Token,
    writer: &mut VMWriter<FenderTypeSystem>,
    scope: &mut LexicalScope,
) -> Result<Expression<FenderTypeSystem>, Box<dyn Error>> {
    let mut left = parse_expr(&token.children[0], writer, scope)?;
    let mut last_end = token.children[0].range.end;
    for right in &token.children[1..] {
        let op: String = right.source[last_end..right.range.start].iter().collect();
        let op = parse_binary_operator(op.trim());
        last_end = right.range.end;
        let right = parse_expr(right, writer, scope)?;
        left = Expression::BinaryOpEval(op, [left, right].into());
    }
    Ok(left)
}

fn parse_value(
    token: &Token,
    writer: &mut VMWriter<FenderTypeSystem>,
    scope: &mut LexicalScope,
) -> Result<Expression<FenderTypeSystem>, Box<dyn Error>> {
    Ok(match token.children[0].get_name().as_deref().unwrap() {
        "literal" => parse_literal(&token.children[0], writer, scope)?,
        "lambdaParameter" => Expression::stack(0),
        "enclosedExpr" => parse_expr(&token.children[0], writer, scope)?,
        "name" => match scope.resolve_propagate(&token.get_match())? {
            VariableType::Captured(addr) => Expression::captured(addr),
            VariableType::Stack(addr) => Expression::stack(addr),
            VariableType::Global(addr) => Expression::global(addr),
        },
        _ => unreachable!(),
    })
}

fn parse_invoke_args(
    token: &Token,
    writer: &mut VMWriter<FenderTypeSystem>,
    scope: &mut LexicalScope,
) -> Result<Vec<Expression<FenderTypeSystem>>, Box<dyn Error>> {
    let token = &token.children[0];
    match token.get_name().as_deref().unwrap() {
        "invokeArgs" => token
            .children_named("expr")
            .map(|arg| parse_expr(arg, writer, scope))
            .collect(),
        "codeBody" => todo!(),
        _ => unreachable!(),
    }
}

fn parse_tail_operation(
    token: &Token,
    expr: Expression<FenderTypeSystem>,
    writer: &mut VMWriter<FenderTypeSystem>,
    scope: &mut LexicalScope,
) -> Result<Expression<FenderTypeSystem>, Box<dyn Error>> {
    let token = &token.children[0];
    match token.get_name().as_deref().unwrap() {
        "invoke" => {
            let args = parse_invoke_args(token, writer, scope)?;
            Ok(Expression::DynamicFunctionCall(expr.into(), args))
        }
        "receiverCall" => {
            let name = token.children[0].get_match();
            let function = match scope.resolve_propagate(&name)? {
                VariableType::Captured(addr) => Expression::captured(addr),
                VariableType::Stack(addr) => Expression::stack(addr),
                VariableType::Global(addr) => Expression::global(addr),
            };
            let mut args = parse_invoke_args(&token.children[1], writer, scope)?;
            args.insert(0, expr);
            Ok(Expression::DynamicFunctionCall(function.into(), args))
        }
        "index" => {
            let pos = parse_expr(&token.children[0], writer, scope)?;
            Ok(Expression::BinaryOpEval(
                FenderBinaryOperator::Index,
                [expr, pos].into(),
            ))
        }
        _ => unreachable!(),
    }
}

fn parse_term(
    token: &Token,
    writer: &mut VMWriter<FenderTypeSystem>,
    scope: &mut LexicalScope,
) -> Result<Expression<FenderTypeSystem>, Box<dyn Error>> {
    let value = token.children_named("value").next().unwrap();
    let mut value = parse_value(value, writer, scope)?;
    if let Some("tailOperationChain") = token.children[token.children.len() - 1]
        .get_name()
        .as_deref()
    {
        let tail_operation_chain = &token.children[token.children.len() - 1];
        for tail_operation in &tail_operation_chain.children {
            value = parse_tail_operation(tail_operation, value, writer, scope)?;
        }
    }
    if let Some("unaryOperator") = token.children[0].get_name().as_deref() {
        let op = match &*token.children[0].get_match() {
            "-" => FenderUnaryOperator::Neg,
            "!" => FenderUnaryOperator::BoolNeg,
            _ => unreachable!(),
        };
        value = Expression::UnaryOpEval(op, value.into());
    }
    Ok(value)
}

fn parse_literal(
    token: &Token,
    writer: &mut VMWriter<FenderTypeSystem>,
    scope: &mut LexicalScope,
) -> Result<Expression<FenderTypeSystem>, Box<dyn Error>> {
    let token = &token.children[0];
    Ok(match token.get_name().as_deref().unwrap() {
        "int" => FenderValue::Int(token.get_match().parse()?).into(),
        "float" => FenderValue::Float(token.get_match().parse()?).into(),
        "boolean" => FenderValue::Bool(token.get_match().parse()?).into(),
        "string" => FenderValue::String(parse_string(token)).into(),
        "list" => parse_list(token, writer, scope)?,
        "null" => FenderValue::Null.into(),
        "closure" => {
            let closure = parse_closure(token, writer, scope)?;
            let expr = match closure.function_type {
                FunctionType::CapturingDef(_) => Expression::FunctionCapture(closure),
                _ => FenderValue::Function(closure).into(),
            };
            expr
        }
        _ => unreachable!(),
    })
}

fn parse_list(
    token: &Token,
    writer: &mut VMWriter<FenderTypeSystem>,
    scope: &mut LexicalScope,
) -> Result<Expression<FenderTypeSystem>, Box<dyn Error>> {
    let mut values = vec![];
    for child in &token.children {
        values.push(parse_expr(child, writer, scope)?);
    }
    Ok(Expression::Initialize(FenderInitializer::List, values))
}

fn parse_string(token: &Token) -> String {
    // TODO: Handle escape sequences
    token.children.iter().map(|t| t.get_match()).collect()
}

fn parse_expr(
    token: &Token,
    writer: &mut VMWriter<FenderTypeSystem>,
    scope: &mut LexicalScope,
) -> Result<Expression<FenderTypeSystem>, Box<dyn Error>> {
    Ok(match token.get_name().as_deref().unwrap() {
        "add" | "mul" | "pow" | "range" | "cmp" | "or" | "and" => {
            parse_binary_operation(token, writer, scope)?
        }
        "term" => parse_term(token, writer, scope)?,
        "expr" | "enclosedExpr" => parse_expr(&token.children[0], writer, scope)?,
        _ => unreachable!(),
    })
}
