use std::{cell::RefCell, collections::HashMap, rc::Rc};

use freight_vm::{
    execution_engine::ExecutionEngine,
    expression::VariableType,
    function::{ArgCount, FunctionWriter},
};

use crate::{
    stdlib::{self, STDLIB_SIZE},
    type_sys::freight_type_system::FenderTypeSystem,
};

use super::{error::InterpreterError, Exports};

#[derive(Debug)]
pub struct LexicalScope<'a> {
    pub(crate) labels: Rc<HashMap<String, usize>>,
    pub(crate) args: ArgCount,
    pub(crate) captures: RefCell<Vec<VariableType>>,
    pub(crate) variables: RefCell<HashMap<String, VariableType>>,
    pub(crate) parent: Option<&'a LexicalScope<'a>>,
    pub(crate) return_target: usize,
    pub(crate) num_stack_vars: usize,
    pub(crate) exports: Exports,
}

impl<'a> LexicalScope<'a> {
    pub fn new(args: ArgCount, return_target: usize) -> LexicalScope<'a> {
        LexicalScope {
            return_target,
            labels: Default::default(),
            captures: Default::default(),
            variables: Default::default(),
            parent: None,
            num_stack_vars: args.stack_size(),
            args,
            exports: Exports::None,
        }
    }

    pub fn child_scope(&self, args: ArgCount, return_target: usize) -> LexicalScope {
        LexicalScope {
            labels: self.labels.clone(),
            args,
            captures: Vec::new().into(),
            variables: HashMap::new().into(),
            parent: Some(self),
            return_target,
            num_stack_vars: args.stack_size(),
            exports: Exports::None,
        }
    }

    pub fn create_stack_var(
        &mut self,
        name: String,
        pos: usize,
    ) -> Result<usize, InterpreterError> {
        let mut variables = self.variables.borrow_mut();
        let existing = variables.get(&name);
        if matches!(
            existing,
            Some(VariableType::Stack(_) | VariableType::Captured(_))
        ) {
            return Err(InterpreterError::DuplicateName(name.to_string(), pos));
        }
        variables.insert(name, VariableType::Stack(self.num_stack_vars));
        self.num_stack_vars += 1;
        Ok(self.num_stack_vars - 1)
    }

    pub fn register_stack_vars(&self, func: &mut FunctionWriter<FenderTypeSystem>) {
        for _ in 0..self.num_stack_vars {
            func.create_variable();
        }
    }

    pub fn capture(&self, name: &str, src_pos: usize) -> Result<(), InterpreterError> {
        let parent_var = self
            .parent
            .and_then(|parent| parent.variables.borrow().get(name).cloned())
            .ok_or_else(|| InterpreterError::UnresolvedName(name.to_string(), src_pos))?;
        let mut captures = self.captures.borrow_mut();
        captures.push(parent_var);
        self.variables
            .borrow_mut()
            .insert(name.to_string(), VariableType::Captured(captures.len() - 1));
        Ok(())
    }

    pub fn resolve_propagate(
        &self,
        engine: &mut ExecutionEngine<FenderTypeSystem>,
        name: &str,
        src_pos: usize,
    ) -> Result<VariableType, InterpreterError> {
        let mut parent_scopes = Vec::new();
        let mut cur = self;
        while !cur.variables.borrow().contains_key(name) {
            parent_scopes.push(cur);
            match &cur.parent {
                Some(parent) => cur = parent,
                None => {
                    if let Some(var) = engine.context.globals.get(name) {
                        return Ok(VariableType::Global(*var));
                    }
                    if let Some(var) = stdlib::load::<STDLIB_SIZE>(name, engine) {
                        return Ok(VariableType::Global(var));
                    } else {
                        return Err(InterpreterError::UnresolvedName(name.to_string(), src_pos));
                    }
                }
            }
        }
        for scope in parent_scopes.into_iter().rev() {
            scope.capture(name, src_pos)?;
        }
        Ok(self.variables.borrow()[name].clone())
    }

    pub fn top_level_return(&self) -> usize {
        if self.parent.is_none() {
            return 0;
        }
        let mut scope = self;
        while scope.parent.and_then(|p| p.parent).is_some() {
            scope = scope.parent.expect("Has parent");
        }
        scope.return_target
    }
}
