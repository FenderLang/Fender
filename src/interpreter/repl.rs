use std::{collections::HashMap, error::Error};

use freight_vm::{
    execution_engine::ExecutionEngine,
    function::{ArgCount, FunctionWriter},
};

use crate::{fender_reference::FenderReference, type_sys::type_system::FenderTypeSystem};

use super::LexicalScope;

pub struct FenderRepl<'a> {
    engine: ExecutionEngine<FenderTypeSystem>,
    scope: LexicalScope<'a>,
}

impl<'a> FenderRepl<'a> {
    pub fn new() -> FenderRepl<'a> {
        let mut engine = ExecutionEngine::new_default();
        let main = FunctionWriter::new(ArgCount::Fixed(0));
        let main_ret = engine.create_return_target();
        let main = engine.register_function(main, main_ret);
        let scope = LexicalScope {
            labels: HashMap::new().into(),
            args: ArgCount::Fixed(0),
            captures: vec![].into(),
            variables: HashMap::new().into(),
            parent: None,
            return_target: main_ret,
            num_stack_vars: 0,
        };
        FenderRepl { engine, scope }
    }

    pub fn run(&mut self, statement: impl AsRef<str>) -> Result<FenderReference, Box<dyn Error>> {
        let token = crate::interpreter::LEXER
            .get()
            .as_ref()
            .unwrap()
            .tokenize_with("statement", statement)?;
        crate::interpreter::parse_statement(&token, &mut self.engine, &mut self.scope, true)?;
        todo!()
    }
}
