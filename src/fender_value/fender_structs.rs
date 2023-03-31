use crate::{fender_reference::{FenderReference, InternalReference}, type_sys::type_id::FenderTypeId};
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct FenderStruct {
    r#type: FenderStructType,
    data: HashMap<usize, InternalReference<FenderReference>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FenderStructType {
    pub name: String,
    pub fields: Vec<String>,
    pub types: Vec<Option<FenderTypeId>>
}
