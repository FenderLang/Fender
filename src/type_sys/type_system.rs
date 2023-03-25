use super::type_id::FenderTypeId;
use crate::{
    fender_reference::FenderReference,
    operators::FenderBinaryOperator,
    operators::FenderInitializer,
    operators::FenderUnaryOperator,
    stdlib::{loader::DependencyList, FenderResource, STDLIB_SIZE},
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
    type GlobalContext = FenderMetadata;
}

#[derive(Default, Debug)]
pub struct FenderMetadata {
    pub deps: DependencyList<STDLIB_SIZE>,
}

impl FenderMetadata {
    pub fn get_dep(&self, dep: FenderResource) -> usize {
        self.deps.0[dep as usize].unwrap()
    }
}
