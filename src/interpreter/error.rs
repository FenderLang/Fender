use std::{error::Error, fmt::Display};

#[derive(Debug)]
pub enum InterpreterError {
    UnresolvedName(String, usize),
    DuplicateName(String, usize),
    UnresolvedLabel(String, usize),
}

impl InterpreterError {
    pub fn src_relative_string(&self, src: &str) -> String {
        let (err_str, pos) = match self {
            InterpreterError::UnresolvedName(name, pos) => {
                (format!("Unresolved name `{name}`"), pos)
            }
            InterpreterError::DuplicateName(name, pos) => (format!("Duplicate name `{name}`"), pos),
            InterpreterError::UnresolvedLabel(name, pos) => {
                (format!("Unresolved label `{name}`"), pos)
            }
        };

        let (line, col) = {
            let src = &src[0..*pos].lines().collect::<Vec<_>>();
            (src.len(), src[src.len() - 1].len())
        };

        format!("{err_str} at line: {line}  col: {col}")
    }
}

impl Display for InterpreterError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnresolvedName(name, pos) => {
                write!(f, "Unresolved name `{}` at position {pos}", name)
            }
            Self::UnresolvedLabel(name, pos) => {
                write!(f, "Unresolved label `{}` at position {pos}", name)
            }
            Self::DuplicateName(name, pos) => {
                write!(f, "Duplicate name `{}` at position {pos}", name)
            }
        }
    }
}

impl Error for InterpreterError {}
