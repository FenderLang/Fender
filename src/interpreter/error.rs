use std::{error::Error, fmt::Display};

#[derive(Debug)]
pub enum InterpreterError {
    UnresolvedName(String),
}

impl Display for InterpreterError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnresolvedName(name) => write!(f, "Unresolved name `{}`", name),
        }
    }
}

impl Error for InterpreterError {}