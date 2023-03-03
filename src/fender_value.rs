use freight_vm::function::FunctionRef;

use crate::{fender_reference::InternalReference, FenderTypeId, FenderTypeSystem};

#[derive(Clone, Debug, Default, PartialEq)]
pub enum FenderValue {
    Ref(InternalReference),
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
    Error(String),
    Function(FunctionRef<FenderTypeSystem>),
    #[default]
    Null,
}

impl FenderValue {
    pub fn get_type_id(&self) -> FenderTypeId {
        match self {
            FenderValue::Ref(_) => FenderTypeId::Reference,
            FenderValue::Int(_) => FenderTypeId::Int,
            FenderValue::Float(_) => FenderTypeId::Float,
            FenderValue::Bool(_) => FenderTypeId::Bool,
            FenderValue::Error(_) => FenderTypeId::Error,
            FenderValue::Null => FenderTypeId::Null,
            FenderValue::Function(_) => FenderTypeId::Function,
            FenderValue::String(_) => FenderTypeId::String,
        }
    }

    pub fn get_real_type_id(&self) -> FenderTypeId {
        match self {
            FenderValue::Ref(val) => val.get_real_type_id(),
            _ => self.get_type_id(),
        }
    }

    pub fn deep_clone(&self) -> FenderValue {
        todo!()
    }
}

impl ToString for FenderValue {
    fn to_string(&self) -> String {
        match self {
            FenderValue::Ref(val) => val.to_string(),
            FenderValue::Int(i) => i.to_string(),
            FenderValue::Float(f) => f.to_string(),
            FenderValue::String(s) => s.clone(),
            FenderValue::Bool(b) => b.to_string(),
            FenderValue::Error(e) => e.clone(),
            FenderValue::Function(_) => "Function".to_string(),
            FenderValue::Null => "null".to_string(),
        }
    }
}