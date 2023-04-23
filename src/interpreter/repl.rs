use std::error::Error;

use freight_vm::{execution_engine::ExecutionEngine, function::ArgCount};
use reedline::{DefaultValidator, Prompt, Reedline, Signal};

use crate::{
    fender_reference::FenderReference, fender_value::FenderValue,
    type_sys::type_system::FenderTypeSystem,
};

use super::LexicalScope;

pub struct FenderRepl<'a> {
    engine: ExecutionEngine<FenderTypeSystem>,
    scope: LexicalScope<'a>,
    editor: Reedline,
}

impl<'a> Default for FenderRepl<'a> {
    fn default() -> Self {
        Self::new()
    }
}

fn display_result(result: Result<Vec<FenderReference>, Box<dyn Error>>) {
    match result {
        Ok(vals) => {
            for val in vals {
                if let FenderValue::Null = &*val {
                    continue;
                }
                println!("{}", val.to_literal_display_string());
            }
        }
        Err(err) => eprintln!("{err:#}"),
    }
}

struct FenderPrompt;

impl Prompt for FenderPrompt {
    fn render_prompt_left(&self) -> std::borrow::Cow<str> {
        "> ".into()
    }

    fn render_prompt_right(&self) -> std::borrow::Cow<str> {
        "".into()
    }

    fn render_prompt_indicator(
        &self,
        _prompt_mode: reedline::PromptEditMode,
    ) -> std::borrow::Cow<str> {
        "".into()
    }

    fn render_prompt_multiline_indicator(&self) -> std::borrow::Cow<str> {
        "..".into()
    }

    fn render_prompt_history_search_indicator(
        &self,
        _history_search: reedline::PromptHistorySearch,
    ) -> std::borrow::Cow<str> {
        "".into()
    }
}

impl<'a> FenderRepl<'a> {
    pub fn new() -> FenderRepl<'a> {
        let mut engine = ExecutionEngine::new_default();
        let scope = LexicalScope::new(ArgCount::Fixed(0), engine.create_return_target());
        FenderRepl {
            engine,
            scope,
            editor: Reedline::create().with_validator(Box::new(DefaultValidator)),
        }
    }

    pub fn run_statement(
        &mut self,
        statement: impl AsRef<str> + Into<String>,
    ) -> Result<Vec<FenderReference>, Box<dyn Error>> {
        let exprs = crate::interpreter::LEXER.get().as_ref().unwrap().tokenize(
            statement.as_ref(),
            |token| {
                crate::interpreter::parsing::parse_statements(
                    &token.children,
                    &mut self.engine,
                    &mut self.scope,
                    super::RegisterVarType::Global,
                )
            },
        )?;
        let exprs = exprs?;
        exprs
            .into_iter()
            .map(|expr| {
                self.engine
                    .evaluate(&expr, &mut [], &[])
                    .map_err(Into::into)
            })
            .collect()
    }

    pub fn run(&mut self) {
        loop {
            let line = self.editor.read_line(&FenderPrompt);
            match line {
                Ok(Signal::Success(line)) => {
                    display_result(self.run_statement(line));
                }
                Ok(Signal::CtrlC) => continue,
                Ok(Signal::CtrlD) => break,
                Err(e) => eprintln!("I/O error: {e}"),
            }
        }
    }
}
