use self::{error::InterpreterError, lexical_scope::LexicalScope, parsing::parse_main_function};
use crate::{
    error::{code_pos::CodePos, parent_type::ParentErrorType, FenderError, FenderResult},
    lazy_cell::LazyCell,
    operators::FenderInitializer,
    type_sys::{
        fender_value::{fender_structs::FenderStructType, FenderValue},
        freight_type_system::FenderTypeSystem,
        type_id::FenderTypeId,
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
    function::{ArgCount, FunctionRef, FunctionWriter},
};
use std::process::exit;

pub mod error;
pub mod lexical_scope;
mod module_loading;
pub mod parsing;
#[cfg(feature = "repl")]
pub mod repl;

pub type InterpreterResult = FenderResult<Expression<FenderTypeSystem>>;

/// Specifies how new declarations should be registered
#[derive(Clone, Copy)]
pub enum RegisterVarType {
    /// Register variables on the stack
    Stack,
    /// Register variables globally
    Global,
    /// Register variables globally, but only register their names within the local scope
    ScopedGlobal(Option<usize>),
}

#[derive(Debug)]
pub enum Exports {
    None,
    All,
    Some(Vec<String>),
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

pub fn create_engine_main(
    source: &str,
) -> FenderResult<(
    ExecutionEngine<FenderTypeSystem>,
    FunctionRef<FenderTypeSystem>,
)> {
    let lex_read = LEXER.get();
    let lex = lex_read.as_ref().unwrap();
    match lex.tokenize(source, |token| parse_main_function(token)) {
        Ok(v) => v,
        Err(e) => Err(Box::new(FenderError {
            error_type: ParentErrorType::Flux(e),
            rust_code_pos: CodePos::new_rel(Some(file!().into()), line!(), column!()),
            fender_code_pos: None,
        })),
    }
}

pub(crate) fn code_body_uses_lambda_parameter(token: &Token) -> bool {
    token
        .rec_iter()
        .ignore_token("codeBody")
        .select_token("lambdaParameter")
        .next()
        .is_some()
}

/// Creates the expression to register a variable.
/// Takes a closure for parsing the expression because variables may be set to expressions that reference themselves,
/// so the name needs to be registered before the expression is parsed.
/// For cases where you already have the parsed expression, ignore both parameters and return it.
pub(crate) fn register_var(
    name: String,
    registration_type: RegisterVarType,
    engine: &mut ExecutionEngine<FenderTypeSystem>,
    scope: &mut LexicalScope,
    expr: impl for<'a, 'b> FnOnce(
        &'a mut ExecutionEngine<FenderTypeSystem>,
        &'a mut LexicalScope<'b>,
    ) -> InterpreterResult,
    pos: usize,
) -> InterpreterResult {
    Ok(match registration_type {
        RegisterVarType::Stack => {
            let var = scope.create_stack_var(name, pos)?;
            let expr = expr(engine, scope)?;
            Expression::AssignStack(var, expr.into())
        }
        RegisterVarType::Global => {
            let global = engine.create_global();
            if engine
                .context
                .globals
                .insert(name.clone(), global)
                .is_some()
            {
                return Err(InterpreterError::DuplicateName(name, pos).into());
            }
            let expr = expr(engine, scope)?;
            Expression::AssignGlobal(global, expr.into())
        }
        RegisterVarType::ScopedGlobal(global_val) => {
            let global = global_val.unwrap_or_else(|| engine.create_global());
            scope.create_stack_var(name.clone(), pos)?;
            scope
                .variables
                .borrow_mut()
                .insert(name, VariableType::Global(global).into());
            let expr = expr(engine, scope)?;
            Expression::AssignGlobal(global, expr.into())
        }
    })
}

pub(crate) fn register_struct(
    name: String,
    fields: Vec<(String, Option<FenderTypeId>)>,
    engine: &mut ExecutionEngine<FenderTypeSystem>,
    scope: &mut LexicalScope,
    registration_type: RegisterVarType,
    pos: usize,
) -> InterpreterResult {
    let fields: Vec<_> = fields
        .into_iter()
        .map(|(name, bound)| {
            let index = engine.context.struct_table.field_index(&name);
            (name, bound, index)
        })
        .collect();
    let field_count = fields.len();
    let new_scope = scope.child_scope(ArgCount::Fixed(field_count), engine.create_return_target());

    let struct_type = FenderStructType { name, fields };
    let mut constructor = FunctionWriter::new(ArgCount::Fixed(field_count));
    let mut exprs = Vec::with_capacity(field_count);
    for i in 0..field_count {
        exprs.push(Expression::Variable(VariableType::Stack(i)));
    }
    constructor.evaluate_expression(Expression::Initialize(
        FenderInitializer::Struct(engine.context.struct_table.len()),
        exprs,
    ));

    engine.context.struct_table.insert(FenderStructType {
        name: struct_type.name.clone(),
        fields: struct_type.fields,
    });

    let function = engine.register_function(constructor, new_scope.return_target);
    register_var(
        struct_type.name,
        registration_type,
        engine,
        scope,
        move |_, _| Ok(Expression::from(FenderValue::Function(function))),
        pos,
    )
}
