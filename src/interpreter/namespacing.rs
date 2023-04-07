use flux_bnf::tokens::Token;
use freight_vm::execution_engine::ExecutionEngine;

use crate::type_sys::type_system::FenderTypeSystem;

pub fn parse_module(
    statements: &Token,
    engine: &mut ExecutionEngine<FenderTypeSystem>,
    mod_name: String,
) {
}
