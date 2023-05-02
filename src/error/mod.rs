use std::fmt::Display;

use crate::interpreter::error::InterpreterError;

use self::{code_pos::CodePos, parent_type::ParentErrorType};

pub mod code_pos;
pub mod parent_type;

pub type FenderResult<T> = Result<T, Box<FenderError>>;

#[derive(Debug)]
pub struct FenderError {
    pub error_type: parent_type::ParentErrorType,
    pub rust_code_pos: code_pos::CodePos,
    pub fender_code_pos: Option<code_pos::CodePos>,
}

impl FenderError {
    pub(crate) fn unwrap_rust<T, E: Into<Box<dyn std::error::Error>>>(
        result: Result<T, E>,
        rust_code_pos: code_pos::CodePos,
        fender_code_pos: Option<code_pos::CodePos>,
    ) -> FenderResult<T> {
        match result {
            Ok(v) => Ok(v),
            Err(rust_e) => Err(Box::new(FenderError::new_rust(
                rust_e.into(),
                rust_code_pos,
                fender_code_pos,
            ))),
        }
    }

    fn new_rust(
        rust_error: Box<dyn std::error::Error>,
        rust_code_pos: code_pos::CodePos,
        fender_code_pos: Option<code_pos::CodePos>,
    ) -> FenderError {
        FenderError {
            error_type: ParentErrorType::RustError(rust_error),
            rust_code_pos,
            fender_code_pos,
        }
    }

    pub fn src_relative_string(&self, src: &str) -> String {
        match &self.error_type {
            ParentErrorType::FenderInterpreterError(e) => e.src_relative_string(src),
            e => format!("{}", e),
        }
    }
}

#[macro_export]
macro_rules! index_oob {
    ($index:expr, $len:expr) => {
        format!(
            "Index out of bounds, index was `{}` but length was `{}`",
            $index, $len
        )
    };
}

#[macro_export]
macro_rules! unwrap_rust {
    ($rust_result:expr) => {
        FenderError::unwrap_rust(
            $rust_result,
            CodePos::new_rel(Some(file!().into()), line!(), column!()),
            None,
        )
    };
    ($rust_result:expr, $code_pos:expr) => {
        FenderError::unwrap_rust(
            $rust_result,
            CodePos::new_rel(Some(file!().into()), line!(), column!()),
            Some($code_pos),
        )
    };
}

impl Display for FenderError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.error_type {
            ParentErrorType::FenderInterpreterError(e) => {
                write!(f, "{}", e,)?;
                if let Some(code_pos) = &self.fender_code_pos {
                    match (&code_pos.file_name, &code_pos.pos) {
                        (None, code_pos::InnerCodePos::Pos(_)) => (),
                        (Some(f_name), code_pos::InnerCodePos::LineCol(l, c)) => {
                            write!(f, ": {f_name} {l}:{c}")?
                        }
                        (Some(f_name), code_pos::InnerCodePos::Pos(p)) => {
                            write!(f, ": {f_name} pos {p}")?
                        }
                        (None, code_pos::InnerCodePos::LineCol(l, c)) => {
                            write!(f, ": position {l}:{c}")?
                        }
                    }
                }
                Ok(())
            }
            ParentErrorType::None => write!(
                f,
                "Error at rust {:?} or fender {:?}",
                self.rust_code_pos, self.fender_code_pos
            ),
            e => write!(f, "{}", e),
        }
    }
}

impl From<InterpreterError> for FenderError {
    fn from(value: InterpreterError) -> Self {
        let pos = match value {
            InterpreterError::UnresolvedName(_, pos)
            | InterpreterError::DuplicateName(_, pos)
            | InterpreterError::NonStructField(_, pos)
            | InterpreterError::UnresolvedLabel(_, pos) => pos,
        };

        FenderError {
            error_type: ParentErrorType::FenderInterpreterError(value),
            rust_code_pos: CodePos::new_abs(None, 0),
            fender_code_pos: Some(CodePos::new_abs(None, pos)),
        }
    }
}

impl From<InterpreterError> for Box<FenderError> {
    fn from(value: InterpreterError) -> Self {
        let pos = match value {
            InterpreterError::UnresolvedName(_, pos)
            | InterpreterError::DuplicateName(_, pos)
            | InterpreterError::NonStructField(_, pos)
            | InterpreterError::UnresolvedLabel(_, pos) => pos,
        };

        Box::new(FenderError {
            error_type: ParentErrorType::FenderInterpreterError(value),
            rust_code_pos: CodePos::new_abs(None, 0),
            fender_code_pos: Some(CodePos::new_abs(None, pos)),
        })
    }
}

impl std::error::Error for FenderError {}
