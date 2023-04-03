use crate::{
    fender_reference::{FenderReference, InternalReference},
    type_sys::type_id::FenderTypeId,
};
use std::{collections::HashMap, rc::Rc};

#[derive(Debug, Clone, PartialEq)]
pub struct FenderStruct {
    pub struct_id: Rc<FenderStructType>,
    pub data: HashMap<i64, InternalReference<FenderReference>>,
}

#[derive(Debug, Clone, PartialEq)]
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
