pub mod error;

use fender::lazy_cell::LazyCell;
use fender::{
    operators::FenderBinaryOperator, operators::FenderUnaryOperator, FenderTypeSystem, FenderValue,
};
use flux_bnf::lexer::{CullStrategy, Lexer};
use flux_bnf::tokens::Token;
use freight_vm::execution_engine::ExecutionEngine;
use freight_vm::expression::Expression;
use freight_vm::function::{FunctionRef, FunctionWriter};
use freight_vm::vm_writer::VMWriter;
use std::cell::RefCell;
use std::collections::HashMap;
use std::error::Error;
use std::rc::Rc;

use self::error::InterpreterError;

#[derive(Clone)]
pub enum VariableType {
    Captured(usize),
    Stack(usize),
}

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
            .insert(name.to_string(), VariableType::Captured(captures.len()));
        Ok(())
    }

    pub fn resolve_propagate(&self, name: &str) -> Result<VariableType, InterpreterError> {
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
        vec!["sep", "lineSep", "lineBreak", "comment"],
        CullStrategy::DeleteAll,
    );
    lex.add_rule_for_names(
        vec!["or", "and", "pow", "mul", "add", "cmp", "range"],
        CullStrategy::LiftAtMost(1),
    );
    lex
});

pub(crate) fn create_vm(source: &str) -> Result<ExecutionEngine<FenderTypeSystem>, Box<dyn Error>> {
    let lex_read = LEXER.get();
    let lex = lex_read.as_ref().unwrap();
    let root = lex.tokenize(source)?;
    println!("{:#?}", root);
    todo!()
}

fn code_body_uses_lambda_parameter(token: &Token) -> bool {
    for child in &token.children {
        match child.get_name().as_deref().unwrap() {
            "lambdaParameter" => return true,
            "codeBody" => return false,
            _ => {
                if code_body_uses_lambda_parameter(child) {
                    return true;
                }
            }
        }
    }
    false
}

fn parse_code_body(
    token: &Token,
    writer: &mut VMWriter<FenderTypeSystem>,
    new_scope: &mut LexicalScope,
) -> Result<FunctionRef<FenderTypeSystem>, Box<dyn Error>> {
    let mut function = FunctionWriter::new(new_scope.args);
    for statement in &token.children {
        let expr = parse_statement(statement, writer, new_scope, &mut function)?;
        function.evaluate_expression(expr);
    }
    Ok(writer.include_function(function))
}

fn parse_statement(
    token: &Token,
    writer: &mut VMWriter<FenderTypeSystem>,
    scope: &mut LexicalScope,
    function: &mut FunctionWriter<FenderTypeSystem>,
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
        "declaration" => {
            let name = token.children[0].get_name().as_deref().unwrap();
            if scope.variables.borrow().contains_key(name) {
                return Err(InterpreterError::DuplicateName(name.to_string()).into());
            }
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

fn parse_binary_operation(
    token: &Token,
    writer: &mut VMWriter<FenderTypeSystem>,
    scope: &mut LexicalScope,
) -> Result<Expression<FenderTypeSystem>, Box<dyn Error>> {
    use FenderBinaryOperator::*;
    let l = &token.children[0];
    let r = &token.children[token.children.len() - 1];
    let op: String = (&l.source[l.range.end..r.range.start]).iter().collect();
    let operator = match &*op {
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
    };
    Ok(Expression::BinaryOpEval(
        operator,
        [parse_expr(l, writer, scope)?, parse_expr(r, writer, scope)?].into(),
    ))
}

fn parse_value(
    token: &Token,
    writer: &mut VMWriter<FenderTypeSystem>,
    scope: &mut LexicalScope,
) -> Result<Expression<FenderTypeSystem>, Box<dyn Error>> {
    Ok(match token.get_name().as_deref().unwrap() {
        "literal" => parse_literal(&token.children[0], writer)?,
        "lambdaParamer" => Expression::stack(0),
        "enclosedExpr" => parse_expr(&token.children[0], writer, scope)?,
        "name" => {
            if let Some(addr) = scope.globals.get(&token.get_match()) {
                return Ok(Expression::Global(*addr));
            }
            match scope.resolve_propagate(&token.get_match())? {
                VariableType::Captured(addr) => Expression::captured(addr),
                VariableType::Stack(addr) => Expression::stack(addr),
            }
        }
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
            .collect::<Result<Vec<_>, _>>(),
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
            };
            let mut args = parse_invoke_args(&token.children[1], writer, scope)?;
            args.insert(0, expr);
            Ok(Expression::DynamicFunctionCall(function.into(), args))
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
    _writer: &mut VMWriter<FenderTypeSystem>,
) -> Result<Expression<FenderTypeSystem>, Box<dyn Error>> {
    Ok(match token.get_name().as_deref().unwrap() {
        "int" => FenderValue::Int(token.get_match().parse()?),
        "float" => FenderValue::Float(token.get_match().parse()?),
        "boolean" => FenderValue::Bool(token.get_match().parse()?),
        "null" => FenderValue::Null,
        _ => unreachable!(),
    }
    .into())
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
        _ => unreachable!(),
    })
}
