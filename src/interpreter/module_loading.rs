use crate::interpreter::CodePos;
use crate::{
    error::{FenderError, FenderResult},
    type_sys::{
        fender_reference::FenderReference, fender_value::FenderValue, type_system::FenderTypeSystem,
    },
    unwrap_rust,
};
use flux_bnf::tokens::Token;
use freight_vm::expression::Expression;
use freight_vm::function::FunctionRef;
use freight_vm::value::Value;
use freight_vm::{execution_engine::ExecutionEngine, function::ArgCount};
use std::path::PathBuf;

use super::error::InterpreterError;
use super::parsing::parse_statements;
use super::{register_struct, register_var, LexicalScope, RegisterVarType};

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
        super::RegisterVarType::ScopedGlobal(None),
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
    let register_expr = register_struct(
        mod_name.clone(),
        fields,
        engine,
        &mut scope,
        super::RegisterVarType::ScopedGlobal(None),
        pos,
    )?;
    unwrap_rust!(engine.evaluate(&register_expr, &mut [], &[]))?;
    let struct_pos = scope.variables.borrow()[&mod_name].clone();
    unwrap_rust!(engine.evaluate(
        &Expression::DynamicFunctionCall(Expression::Variable(struct_pos).into(), field_locs),
        &mut [],
        &[],
    ))
}

enum ImportTarget {
    /// Directly import the target, packed into a struct
    Direct,
    /// Import all of the fields of the target individually
    All,
    /// Import a single field from the target
    Field(String),
    /// Import targets within a field on this target
    UnpackField(String, Box<ImportTarget>),
    /// Import multiple specific targets
    Multi(Vec<ImportTarget>),
}

fn get_struct_field(
    val: &FenderReference,
    engine: &mut ExecutionEngine<FenderTypeSystem>,
    name: &str,
    pos: usize,
) -> FenderResult<FenderReference> {
    let FenderValue::Struct(val) = &**val else {
        return Err(InterpreterError::NonStructField(name.to_string(), pos).into())
    };
    let index = engine.context.struct_table.field_index(name);
    let field = val
        .data
        .get(&(index as i64))
        .ok_or_else(|| InterpreterError::UnresolvedName(name.to_string(), pos))?;
    Ok(field.dupe_ref())
}

fn parse_import_target(token: &Token) -> ImportTarget {
    if token.children.is_empty() {
        return ImportTarget::All;
    }
    let child = &token.children[0];
    match child.get_name().as_deref().unwrap() {
        "name" => {
            let name = child.get_match();
            if token.children.len() == 1 {
                ImportTarget::Field(name)
            } else {
                ImportTarget::UnpackField(name, parse_import_target(&token.children[1]).into())
            }
        }
        "importTargetMulti" => {
            ImportTarget::Multi(child.children.iter().map(parse_import_target).collect())
        }
        name => unreachable!("{}", name),
    }
}

pub(crate) fn parse_import(
    token: &Token,
    engine: &mut ExecutionEngine<FenderTypeSystem>,
    scope: &mut LexicalScope,
    registration_type: RegisterVarType,
) -> FenderResult<()> {
    let filename = &token.children[0].get_match();
    let mut path = PathBuf::from(filename);
    let var_name = path
        .file_name()
        .and_then(|p| p.to_str())
        .expect("Empty path")
        .to_string();
    path.set_extension("fndr");
    let path = path
        .canonicalize()
        .unwrap_or_else(|_| panic!("Invalid file path {:?}", path));
    let module = if let Some(loc) = engine.context.imports.get(&path) {
        unwrap_rust!(engine.evaluate(&Expression::global(*loc), &mut [], &[]))?
    } else {
        let contents = unwrap_rust!(std::fs::read_to_string(&path))?;
        let lexer = crate::interpreter::LEXER.get();
        let module =
            unwrap_rust!(lexer
                .as_ref()
                .unwrap()
                .tokenize(contents, |token| parse_module(
                    &token.children,
                    engine,
                    var_name.to_string(),
                    0
                )))?;
        module?
    };
    let targets = if token.children.len() == 1 {
        ImportTarget::Direct
    } else {
        parse_import_target(&token.children[1])
    };
    let pos = token.range.start;
    import_targets(
        &module,
        var_name,
        engine,
        scope,
        registration_type,
        &targets,
        pos,
    )?;
    Ok(())
}

fn import_targets(
    module: &FenderReference,
    name: String,
    engine: &mut ExecutionEngine<FenderTypeSystem>,
    scope: &mut LexicalScope,
    registration_type: RegisterVarType,
    targets: &ImportTarget,
    pos: usize,
) -> FenderResult<()> {
    let mut exprs = vec![];
    match targets {
        ImportTarget::Direct => {
            let expr = register_var(
                name,
                registration_type,
                engine,
                scope,
                |_, _| Ok(Expression::RawValue(module.dupe_ref())),
                pos,
            )?;
            exprs.push(expr);
        }
        ImportTarget::All => {
            let FenderValue::Struct(module) = &**module else {
                return Err(InterpreterError::NonStructField("*".to_string(), pos).into());
            };
            for field in &module.struct_id.fields {
                let (field_name, _, field_index) = field;
                let data = module.data.get(&(*field_index as i64)).unwrap().dupe_ref();
                let expr = register_var(
                    field_name.clone(),
                    registration_type,
                    engine,
                    scope,
                    |_, _| Ok(Expression::RawValue(data)),
                    pos,
                )?;
                exprs.push(expr);
            }
        }
        ImportTarget::Field(name) => {
            let data = get_struct_field(module, engine, name, pos)?;
            let expr = register_var(
                name.to_string(),
                registration_type,
                engine,
                scope,
                |_, _| Ok(Expression::RawValue(data)),
                pos,
            )?;
            exprs.push(expr);
        }
        ImportTarget::UnpackField(name, target) => {
            let data = get_struct_field(module, engine, name, pos)?;
            import_targets(
                &data,
                name.to_string(),
                engine,
                scope,
                registration_type,
                target,
                pos,
            )?;
        }
        ImportTarget::Multi(targets) => {
            for target in targets {
                import_targets(
                    module,
                    name.clone(),
                    engine,
                    scope,
                    registration_type,
                    target,
                    pos,
                )?;
            }
        }
    }
    for expr in exprs {
        unwrap_rust!(engine.evaluate(&expr, &mut [], &[]))?;
    }
    Ok(())
}

pub(crate) fn parse_plugin(
    token: &Token,
    engine: &mut ExecutionEngine<FenderTypeSystem>,
    scope: &mut LexicalScope,
) -> FenderResult<()> {
    let file_name = token.children[0].get_match();
    let mut exprs = Vec::new();
    unsafe {
        engine
            .context
            .plugin_manager
            .load_plugin(&file_name)
            .unwrap();

        let plugins = engine.context.plugin_manager.plugins();

        let function_parts = plugins[plugins.len() - 1]
            .get_functions()
            .iter()
            .map(|(name, (native_func, arg_count))| {
                ((*name).to_owned(), (*native_func).clone(), *arg_count)
            })
            .collect::<Vec<_>>();

        let value_parts = plugins[plugins.len() - 1]
            .get_values()
            .iter()
            .map(|(name, value)| ((*name).to_owned(), (*value).clone()))
            .collect::<Vec<_>>();

        for (name, native_func, arg_count) in function_parts.into_iter() {
            let global = engine.create_global();
            let expr = register_var(
                name,
                RegisterVarType::ScopedGlobal(Some(global)),
                engine,
                scope,
                |_, _| {
                    Ok(Expression::RawValue(
                        FenderValue::Function(FunctionRef::new_native(
                            global,
                            native_func.to_owned(),
                            arg_count,
                        ))
                        .into(),
                    ))
                },
                token.range.start,
            )?;
            exprs.push(expr);
        }

        for (name, value) in value_parts.into_iter() {
            let expr = register_var(
                name,
                RegisterVarType::ScopedGlobal(None),
                engine,
                scope,
                |_, _| Ok(Expression::RawValue(value.clone().into())),
                token.range.start,
            )?;
            exprs.push(expr);
        }
    };
    for expr in exprs {
        unwrap_rust!(engine.evaluate(&expr, &mut [], &[]))?;
    }
    Ok(())
}
