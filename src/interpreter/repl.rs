use std::{collections::HashMap, error::Error, io::Write};

use freight_vm::{execution_engine::ExecutionEngine, function::ArgCount};

use crate::{
    fender_reference::FenderReference, fender_value::FenderValue,
    type_sys::type_system::FenderTypeSystem,
};

use super::LexicalScope;

pub struct FenderRepl<'a> {
    engine: ExecutionEngine<FenderTypeSystem>,
    scope: LexicalScope<'a>,
}

impl<'a> Default for FenderRepl<'a> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> FenderRepl<'a> {
    pub fn new() -> FenderRepl<'a> {
        let mut engine = ExecutionEngine::new_default();
        let scope = LexicalScope {
            labels: HashMap::new().into(),
            args: ArgCount::Fixed(0),
            captures: vec![].into(),
            variables: HashMap::new().into(),
            parent: None,
            return_target: engine.create_return_target(),
            num_stack_vars: 0,
        };
        FenderRepl { engine, scope }
    }

    pub fn run_statement(
        &mut self,
        statement: impl AsRef<str>,
    ) -> Result<FenderReference, Box<dyn Error>> {
        let token = crate::interpreter::LEXER
            .get()
            .as_ref()
            .unwrap()
            .tokenize_with("statement", statement)?;
        let expr =
            crate::interpreter::parse_statement(&token, &mut self.engine, &mut self.scope, true)?;
        Ok(self.engine.evaluate(&expr, &mut [], &[])?)
    }

    fn prompt(&self) {
        print!("> ");
        std::io::stdout().flush().unwrap();
    }

    pub fn run(&mut self) {
        self.prompt();
        while let Some(line) = std::io::stdin().lines().next() {
            let line = line.unwrap();
            match self.run_statement(line) {
                Ok(FenderReference::FRaw(FenderValue::Null)) => (),
                Ok(val) => println!("{}", val.to_string()),
                Err(err) => eprintln!("{err:#}"),
            }
            self.prompt();
        }
    }
}
