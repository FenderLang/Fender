use super::type_id::FenderTypeId;
use crate::operators::FenderBinaryOperator;
use crate::operators::FenderUnaryOperator;
use crate::FenderReference;

use freight_vm::TypeSystem;

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct FenderTypeSystem;

impl TypeSystem for FenderTypeSystem {
    type Value = FenderReference;
    type BinaryOp = FenderBinaryOperator;
    type UnaryOp = FenderUnaryOperator;
    type TypeId = FenderTypeId;
}
