use fender::lazy_cell::LazyCell;
use fender::FenderTypeSystem;
use flux_bnf::lexer::{CullStrategy, Lexer};
use flux_bnf::tokens::Token;
use freight_vm::execution_engine::ExecutionEngine;
use freight_vm::expression::Expression;
use freight_vm::vm_writer::VMWriter;
use std::error::Error;

static LEXER: LazyCell<Lexer> = LazyCell::new(|| {
    let mut lex = flux_bnf::bnf::parse(include_str!("../fender.bnf")).expect("Invalid BNF");
    lex.set_unnamed_rule(CullStrategy::LiftChildren);
    lex.add_rule_for_names(
        vec!["sep", "lineSep", "lineBreak", "comment"],
        CullStrategy::DeleteAll,
    );
    lex.add_rule_for_names(
        vec!["or", "and", "pow", "mul", "add", "cmp", "range"],
        CullStrategy::LiftAtMost(1),
    );
    lex.add_rule_for_names(vec!["term", "literal", "value"], CullStrategy::LiftChildren);
    lex
});

pub(crate) fn create_vm(source: &str) -> Result<ExecutionEngine<FenderTypeSystem>, Box<dyn Error>> {
    let lex_read = LEXER.get();
    let lex = lex_read.as_ref().unwrap();
    let root = lex.tokenize(source)?;
    println!("{:#?}", root);
    todo!()
}

fn parse_expr(
    token: &Token,
    writer: &mut VMWriter<FenderTypeSystem>,
) -> Expression<FenderTypeSystem> {
    match token.get_name().as_ref().unwrap() {
        _ => unreachable!(),
    }
    todo!()
}
