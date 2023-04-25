use super::{
    fender_reference::FenderReference, fender_value::fender_structs::FenderStructType,
    type_id::FenderTypeId,
};
use crate::{
    fender_value::FenderValue,
    operators::FenderBinaryOperator,
    operators::FenderInitializer,
    operators::FenderUnaryOperator,
    plugin::PluginManager,
    stdlib::{loader::DependencyList, FenderResource, STDLIB_SIZE},
};
use freight_vm::{expression::Expression, TypeSystem};
use std::{collections::HashMap, path::PathBuf, rc::Rc};

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct FenderTypeSystem;

impl TypeSystem for FenderTypeSystem {
    type Value = FenderReference;
    type BinaryOp = FenderBinaryOperator;
    type UnaryOp = FenderUnaryOperator;
    type TypeId = FenderTypeId;
    type Init = FenderInitializer;
    type GlobalContext = FenderGlobalContext;
}

impl From<FenderValue> for Expression<FenderTypeSystem> {
    fn from(value: FenderValue) -> Self {
        Expression::RawValue(value.into())
    }
}

#[derive(Default, Debug)]
pub struct FenderGlobalContext {
    pub deps: DependencyList<STDLIB_SIZE>,
    pub struct_table: StructTable,
    pub globals: HashMap<String, usize>,
    pub imports: HashMap<PathBuf, usize>,
    pub plugin_manager: PluginManager,
}

impl FenderGlobalContext {
    pub fn get_dep(&self, dep: FenderResource) -> usize {
        self.deps.0[dep as usize].unwrap()
    }
}

#[derive(Default, Debug)]
pub struct StructTable {
    type_list: Vec<Rc<FenderStructType>>,
    struct_name_index: HashMap<String, usize>,
    field_map: HashMap<String, usize>,
}

impl StructTable {
    pub fn type_list(&mut self) -> &Vec<Rc<FenderStructType>> {
        &self.type_list
    }
    pub fn struct_name_index(&self) -> &HashMap<String, usize> {
        &self.struct_name_index
    }

    pub fn insert(&mut self, new_type: FenderStructType) {
        self.struct_name_index
            .insert(new_type.name.clone(), self.type_list.len());
        self.type_list.push(Rc::new(new_type));
    }

    pub(crate) fn len(&self) -> usize {
        self.type_list.len()
    }

    pub(crate) fn field_index(&mut self, name: &str) -> usize {
        let next = self.field_map.len();

        match self.field_map.get(name) {
            Some(v) => *v,
            None => {
                self.field_map.insert(name.to_string(), next);
                next
            }
        }
    }
}
