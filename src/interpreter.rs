use std::error::Error;
use flux_bnf::lexer::{CullStrategy, Lexer};
use flux_bnf::tokens::Token;
use freight_vm::execution_engine::ExecutionEngine;
use freight_vm::expression::Expression;
use freight_vm::vm_writer::VMWriter;
use fender::FenderTypeSystem;

fn create_lexer() -> Lexer {
    let mut lexer = flux_bnf::bnf::parse(include_str!("../fender.bnf")).expect("Invalid BNF");
    lexer.set_unnamed_rule(CullStrategy::LiftChildren);
    lexer.add_rule_for_names(vec![
        "sep",
        "lineSep",
        "lineBreak",
        "comment",
    ], CullStrategy::DeleteAll);
    lexer.add_rule_for_names(vec![
        "or",
        "and",
        "pow",
        "mul",
        "add",
        "cmp",
        "range",
    ], CullStrategy::LiftAtMost(1));
    lexer
}

pub(crate) fn create_vm(source: &str) -> Result<ExecutionEngine<FenderTypeSystem>, Box<dyn Error>> {
    let lexer = create_lexer();
    let root = lexer.tokenize(source)?;
    println!("{:#?}", root);
    todo!()
}

fn parse_expr(token: &Token, writer: &mut VMWriter<FenderTypeSystem>) -> Expression<FenderTypeSystem> {
    match token.get_name().as_ref().unwrap() {

        _ => unreachable!(),
    }
    todo!()
}