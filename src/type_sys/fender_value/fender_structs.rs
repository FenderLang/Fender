use crate::type_sys::{
    fender_reference::{FenderReference, InternalReference},
    type_id::FenderTypeId,
};
use std::{collections::HashMap, rc::Rc};

#[derive(Debug, Clone, PartialEq)]
pub struct FenderStruct {
    pub struct_id: Rc<FenderStructType>,
    pub data: HashMap<i64, InternalReference<FenderReference>>,
}

impl std::hash::Hash for FenderStruct {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.struct_id.hash(state);
        self.data.iter().for_each(|(k, v)| {
            k.hash(state);
            v.hash(state)
        });
    }
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct FenderStructType {
    pub name: String,
    pub fields: Vec<(String, Option<FenderTypeId>, usize)>,
}

impl ToString for FenderStruct {
    fn to_string(&self) -> String {
        format!(
            "{}{{{}}}",
            self.struct_id.name,
            self.struct_id
                .fields
                .iter()
                .map(|(name, _, i)| format!(
                    "{}:{}",
                    name,
                    (*self.data[&(*i as i64)]).to_literal_display_string()
                ))
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}
