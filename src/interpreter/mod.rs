pub mod error;

use fender::lazy_cell::LazyCell;
use fender::{FenderBinaryOperator, FenderTypeSystem, FenderValue};
use flux_bnf::lexer::{CullStrategy, Lexer};
use flux_bnf::tokens::Token;
use freight_vm::execution_engine::ExecutionEngine;
use freight_vm::expression::Expression;
use freight_vm::vm_writer::VMWriter;
use std::collections::HashMap;
use std::error::Error;

use self::error::InterpreterError;

pub enum VariableType {
    Captured(usize),
    Stack(usize),
}

pub struct LexicalScope<'a> {
    variables: HashMap<String, VariableType>,
    parent: Option<Box<&'a mut LexicalScope<'a>>>,
}

impl<'a> LexicalScope<'a> {
    pub fn resolve(&mut self, name: &str) -> Result<VariableType, InterpreterError> {
        todo!()
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

fn parse_binary_operation_token(
    token: &Token,
    writer: &mut VMWriter<FenderTypeSystem>,
) -> Expression<FenderTypeSystem> {
    let l = &token.children[0];
    let r = &token.children[token.children.len() - 1];
    let op: String = (&l.source[l.range.end..r.range.start]).iter().collect();
    parse_binary_operation(&op, l, r, writer)
}

fn parse_binary_operation(
    op: &str,
    l: &Token,
    r: &Token,
    writer: &mut VMWriter<FenderTypeSystem>,
) -> Expression<FenderTypeSystem> {
    let operator = match op {
        "+" => FenderBinaryOperator::Add,
        "-" => FenderBinaryOperator::Sub,
        "*" => FenderBinaryOperator::Mul,
        "/" => FenderBinaryOperator::Div,
        "&&" => FenderBinaryOperator::And,
        "||" => FenderBinaryOperator::Or,
        ">" => FenderBinaryOperator::Gt,
        _ => unreachable!(),
    };
    Expression::BinaryOpEval(
        operator,
        [parse_expr(l, writer), parse_expr(r, writer)].into(),
    )
}

fn parse_value(
    token: &Token,
    writer: &mut VMWriter<FenderTypeSystem>,
    scope: &mut LexicalScope,
) -> Result<Expression<FenderTypeSystem>, Box<dyn Error>> {
    Ok(match token.get_name().as_deref().unwrap() {
        "literal" => parse_literal(&token.children[0], writer)?,
        "lambdaParamer" => Expression::Variable(0),
        "enclosedExpr" => parse_expr(&token.children[0], writer),
        "name" => match scope.resolve(&token.get_match())? {
            VariableType::Captured(addr) => Expression::CapturedValue(addr),
            VariableType::Stack(addr) => Expression::Variable(addr),
        },
        _ => unreachable!(),
    })
}

fn parse_literal(
    token: &Token,
    _writer: &mut VMWriter<FenderTypeSystem>,
) -> Result<Expression<FenderTypeSystem>, Box<dyn Error>> {
    match token.get_name().as_deref().unwrap() {
        "int" => FenderValue::Int(token.get_match().parse()?),
        "float" => FenderValue::Float(token.get_match().parse()?),
        "boolean" => FenderValue::Bool(token.get_match().parse()?),
        "null" => FenderValue::Null,
        _ => unreachable!(),
    };
    todo!()
}

fn parse_expr(
    token: &Token,
    writer: &mut VMWriter<FenderTypeSystem>,
) -> Expression<FenderTypeSystem> {
    match token.get_name().as_deref().unwrap() {
        "add" | "mul" | "pow" | "range" | "cmp" | "or" | "and" => {
            parse_binary_operation_token(token, writer)
        }
        _ => unreachable!(),
    };
    todo!()
}
