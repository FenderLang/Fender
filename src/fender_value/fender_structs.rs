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
