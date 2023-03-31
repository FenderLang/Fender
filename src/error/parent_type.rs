use std::fmt::Display;

/// Wrap the different types of errors that `FenderError` could hold
#[derive(Debug, Default)]
pub enum ParentErrorType {
    FenderInterpreterError(crate::interpreter::error::InterpreterError),
    Flux(flux_bnf::error::FluxError),
    Freight(freight_vm::error::FreightError),
    RustError(Box<dyn std::error::Error>),
    #[default]
    None,
}

impl Display for ParentErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParentErrorType::FenderInterpreterError(e) => write!(f, "{}", e),
            ParentErrorType::Flux(e) => write!(f, "{}", e),
            ParentErrorType::Freight(e) => write!(f, "{}", e),
            ParentErrorType::RustError(e) => write!(f, "{}", e),
            ParentErrorType::None => write!(f, "NONE"),
        }
    }
}
