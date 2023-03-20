use super::type_id::FenderTypeId;
use crate::{
    fender_reference::FenderReference, operators::FenderBinaryOperator,
    operators::FenderInitializer, operators::FenderUnaryOperator,
};
use freight_vm::TypeSystem;

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct FenderTypeSystem;

impl TypeSystem for FenderTypeSystem {
    type Value = FenderReference;
    type BinaryOp = FenderBinaryOperator;
    type UnaryOp = FenderUnaryOperator;
    type TypeId = FenderTypeId;
    type Init = FenderInitializer;
}
