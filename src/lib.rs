#![allow(dead_code)]

use freight_vm::TypeSystem;

// move these to a prelude or just get rid of them
pub use fender_reference::FenderReference;
pub use fender_value::FenderValue;
pub use type_sys::{type_id::TypeId, type_system::FenderTypeSystem};

pub mod fender_reference;
pub mod fender_value;
pub mod type_sys {
    pub mod type_id;
    pub mod type_system;
}

#[derive(Debug, Clone)]
pub enum FenderBinaryOperator {
    Add,
    Sub,
    Div,
    Mod,
    Mul,
    And,
    Or,
}

#[derive(Debug, Clone)]
pub enum FenderUnaryOperator {
    Neg,
    BoolNeg,
}
