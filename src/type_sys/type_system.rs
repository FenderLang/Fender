use super::type_id::FenderTypeId;
use crate::{FenderBinaryOperator, FenderReference, FenderUnaryOperator};
use freight_vm::TypeSystem;

pub struct FenderTypeSystem;

impl TypeSystem for FenderTypeSystem {
    type Value = FenderReference;
    type BinaryOp = FenderBinaryOperator;
    type UnaryOp = FenderUnaryOperator;
    type TypeId = FenderTypeId;
}