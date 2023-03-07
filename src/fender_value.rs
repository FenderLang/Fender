use freight_vm::function::FunctionRef;

use crate::{fender_reference::InternalReference, FenderReference, FenderTypeId, FenderTypeSystem};

#[derive(Clone, Debug, Default, PartialEq)]
pub enum FenderValue {
    Ref(InternalReference),
    Int(i64),
    Float(f64),
    Char(char),
    String(String),
    Bool(bool),
    Error(String),
    Function(FunctionRef<FenderTypeSystem>),
    List(Vec<FenderReference>),
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
            FenderValue::Char(_) => FenderTypeId::Char,
            FenderValue::Error(_) => FenderTypeId::Error,
            FenderValue::Null => FenderTypeId::Null,
            FenderValue::Function(_) => FenderTypeId::Function,
            FenderValue::String(_) => FenderTypeId::String,
            FenderValue::List(_) => FenderTypeId::List,
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

    pub fn len(&self) -> Result<usize, String> {
        Ok(match self {
            FenderValue::Ref(v) => v.len()?,
            FenderValue::String(s) => s.len(),
            FenderValue::List(v) => v.len(),
            e => {
                return Err(format!(
                    "cannot get length of type {}",
                    e.get_type_id().to_string()
                ))
            }
        })
    }

    /// return a raw `FenderValue` unwrapping all references
    pub fn unwrap_value(&self) -> FenderValue {
        match self {
            FenderValue::Ref(r) => r.unwrap_value(),
            v => v.clone(),
        }
    }
}

impl ToString for FenderValue {
    fn to_string(&self) -> String {
        match self {
            FenderValue::Ref(val) => val.to_string(),
            FenderValue::Int(i) => i.to_string(),
            FenderValue::Float(f) => f.to_string(),
            FenderValue::Char(c) => c.to_string(),
            FenderValue::String(s) => s.clone(),
            FenderValue::Bool(b) => b.to_string(),
            FenderValue::Error(e) => e.clone(),
            FenderValue::Function(_) => "Function".to_string(),
            FenderValue::Null => "null".to_string(),
            FenderValue::List(list) => format!(
                "[{}]",
                list.iter()
                    .map(|e| e.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }
}
