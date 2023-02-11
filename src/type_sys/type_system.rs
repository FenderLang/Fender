use super::type_id::TypeId;
use crate::{FenderBinaryOperator, FenderReference, FenderUnaryOperator};
use freight_vm::TypeSystem;

pub struct FenderTypeSystem;

impl TypeSystem for FenderTypeSystem {
    type V = FenderReference;
    type B = FenderBinaryOperator;
    type U = FenderUnaryOperator;
    type T = TypeId;
}
