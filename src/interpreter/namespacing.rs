use std::path::PathBuf;

use flux_bnf::tokens::Token;
use freight_vm::expression::{Expression, VariableType};
use freight_vm::{execution_engine::ExecutionEngine, function::ArgCount};

use crate::fender_reference::FenderReference;
use crate::interpreter::CodePos;
use crate::{
    error::{FenderError, FenderResult},
    type_sys::type_system::FenderTypeSystem,
    unwrap_rust,
};

use super::{parse_statements, register_struct, register_var, LexicalScope, RegisterVarType};

pub(crate) fn parse_module(
    statements: &[Token],
    engine: &mut ExecutionEngine<FenderTypeSystem>,
    mod_name: String,
    pos: usize,
) -> FenderResult<FenderReference> {
    let mut scope = LexicalScope::new(ArgCount::Fixed(0), 0);
    for statement in parse_statements(
        statements,
        engine,
        &mut scope,
        super::RegisterVarType::AnonymousGlobal,
    )? {
        unwrap_rust!(engine.evaluate(&statement, &mut [], &[]))?;
    }
    let fields = match &mut scope.exports {
        super::Exports::None => vec![],
        super::Exports::All => scope.variables.borrow().keys().cloned().collect(),
        super::Exports::Some(fields) => std::mem::take(fields),
    };
    let field_locs: Vec<Expression<FenderTypeSystem>> = fields
        .iter()
        .map(|f| Expression::Variable(scope.variables.borrow()[f].clone()))
        .collect();
    let fields = fields.into_iter().map(|field| (field, None)).collect();
    register_struct(
        mod_name.clone(),
        fields,
        engine,
        &mut scope,
        super::RegisterVarType::AnonymousGlobal,
        pos,
    )?;
    let struct_pos = scope.variables.borrow()[&mod_name].clone();
    unwrap_rust!(engine.evaluate(
        &Expression::DynamicFunctionCall(Expression::Variable(struct_pos).into(), field_locs),
        &mut [],
        &[],
    ))
}

pub(crate) fn parse_import(
    token: &Token,
    engine: &mut ExecutionEngine<FenderTypeSystem>,
    scope: &mut LexicalScope,
    registration_type: RegisterVarType,
) -> FenderResult<()> {
    let filename = &token.children[0].get_match();
    let mut path = PathBuf::from(filename)
        .canonicalize()
        .expect("Invalid filepath format");
    let var_name = path
        .file_name()
        .and_then(|p| p.to_str())
        .expect("Empty path")
        .to_string();
    path.set_extension("fndr");
    if let Some(loc) = engine.context.imports.get(&path) {
        scope
            .variables
            .borrow_mut()
            .insert(var_name, VariableType::Global(*loc));
        return Ok(());
    }
    let contents = unwrap_rust!(std::fs::read_to_string(&path))?;
    let lexer = crate::interpreter::LEXER.get();
    let token = unwrap_rust!(lexer.as_ref().unwrap().tokenize(contents))?;
    let module = parse_module(&token.children, engine, var_name.to_string(), 0)?;
    let expr = register_var(
        var_name,
        registration_type,
        engine,
        scope,
        |_, _| Ok(Expression::RawValue(module)),
        token.range.start,
    )?;
    engine.evaluate(&expr, &mut [], &[]).unwrap();
    Ok(())
}
