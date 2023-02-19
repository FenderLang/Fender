#![allow(dead_code)]

use freight_vm::TypeSystem;

// move these to a prelude or just get rid of them
pub use fender_reference::FenderReference;
pub use fender_value::FenderValue;
pub use type_sys::{type_id::FenderTypeId, type_system::FenderTypeSystem};

pub mod fender_reference;
pub mod fender_value;
pub mod type_sys {
    pub mod type_id;
    pub mod type_system;
}
pub mod stdlib;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FenderBinaryOperator {
    Add,
    Sub,
    Div,
    Mod,
    Mul,
    And,
    Or,
    Gt,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FenderUnaryOperator {
    Neg,
    BoolNeg,
}

#[macro_export]
macro_rules! count {
    ($first:pat_param, $($rest:pat_param),*) => {
        1 + $crate::count!($($rest),*)
    };
    ($first:pat_param) => {1};
    () => {0};
}

#[macro_export]
macro_rules! fndr_native_func {
    ($name:ident, | $ctx:tt $(, $($arg:pat_param),*)? | $body:expr) => {
        pub fn $name($ctx: &mut freight_vm::execution_context::ExecutionContext<$crate::type_sys::type_system::FenderTypeSystem>, args: [$crate::fender_reference::FenderReference; $crate::count!($($($arg),*)?)]) -> Result<$crate::fender_reference::FenderReference, freight_vm::error::FreightError> {
            $(
                    let [$($arg),*] = args;
                    )?
            $body
        }
    }
}
