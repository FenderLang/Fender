use rustyline::{error::ReadlineError, DefaultEditor};
use std::error::Error;

use freight_vm::{execution_engine::ExecutionEngine, function::ArgCount};

use crate::{
    fender_reference::FenderReference, fender_value::FenderValue,
    type_sys::type_system::FenderTypeSystem,
};

use super::LexicalScope;

pub struct FenderRepl<'a> {
    engine: ExecutionEngine<FenderTypeSystem>,
    scope: LexicalScope<'a>,
    editor: DefaultEditor,
    buffer: String,
}

impl<'a> Default for FenderRepl<'a> {
    fn default() -> Self {
        Self::new()
    }
}

fn display_result(result: Result<FenderReference, Box<dyn Error>>) {
    match result {
        Ok(FenderReference::FRaw(FenderValue::Null)) => (),
        Ok(val) => println!("{}", val.to_string()),
        Err(err) => eprintln!("{err:#}"),
    }
}

impl<'a> FenderRepl<'a> {
    pub fn new() -> FenderRepl<'a> {
        let mut engine = ExecutionEngine::new_default();
        let scope = LexicalScope::new(ArgCount::Fixed(0), engine.create_return_target());
        FenderRepl {
            engine,
            scope,
            editor: DefaultEditor::new().expect("Prompt created"),
            buffer: String::new(),
        }
    }

    pub fn run_statement(
        &mut self,
        statement: impl AsRef<str> + Into<String>,
    ) -> Result<FenderReference, Box<dyn Error>> {
        let token = crate::interpreter::LEXER
            .get()
            .as_ref()
            .unwrap()
            .tokenize_with("statement", statement.as_ref())?;
        self.editor
            .add_history_entry(statement)
            .expect("Add history entry");
        let expr =
            crate::interpreter::parse_statement(&token, &mut self.engine, &mut self.scope, true)?;
        Ok(self.engine.evaluate(&expr, &mut [], &[])?)
    }

    fn is_balanced(&self) -> bool {
        let mut depth = 0;
        let mut quoted = false;
        let mut escape = false;
        for c in self.buffer.chars() {
            if escape {
                escape = false;
                continue;
            }
            match c {
                '\\' => escape = true,
                '"' => quoted = !quoted,
                '{' | '[' | '(' if !quoted => depth += 1,
                '}' | ']' | ')' if !quoted => depth -= 1,
                _ => (),
            }
        }
        depth == 0 && !quoted
    }

    pub fn run(&mut self) {
        loop {
            let prompt = if self.buffer.is_empty() { "> " } else { ">>" };
            let line = self.editor.readline(prompt);
            match line {
                Ok(line) => {
                    self.buffer.push_str(&line);
                    if !self.is_balanced() {
                        self.buffer.push('\n');
                        continue;
                    }
                    let statement = std::mem::take(&mut self.buffer);
                    display_result(self.run_statement(statement.trim()));
                }
                Err(ReadlineError::Interrupted) => self.buffer.clear(),
                Err(ReadlineError::Io(e)) => eprintln!("I/O error: {e}"),
                Err(ReadlineError::WindowResized) => continue,
                Err(ReadlineError::Decode(e)) => eprintln!("Could not decode input: {e}"),
                Err(ReadlineError::SystemError(e)) => eprintln!("System error: {e}"),
                Err(ReadlineError::Eof) => {
                    println!("\nGoodbye");
                    break;
                }
                Err(e) => eprintln!("Unknown error: {e}"),
            }
        }
    }
}
