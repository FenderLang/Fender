use crate::{
    fender_reference::{FenderReference, InternalReference},
    type_sys::{type_id::FenderTypeId, type_system::FenderTypeSystem},
};
use freight_vm::{function::FunctionRef, value::Value};
use rand::{seq::SliceRandom, thread_rng};

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
    pub fn make_error<S: Into<String>>(e_body: S) -> FenderValue {
        FenderValue::Error(e_body.into())
    }

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
        use FenderValue::*;
        match self {
            Ref(v) => Ref(InternalReference::new(v.deep_clone())),
            Int(i) => Int(*i),
            Float(f) => Float(*f),
            Char(c) => Char(*c),
            String(s) => String(s.clone()),
            Bool(b) => Bool(*b),
            Error(e) => Error(e.clone()),
            Function(f) => Function(f.clone()),
            List(l) => List(l.iter().map(Value::deep_clone).collect()),
            Null => Null,
        }
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

    pub fn shuffle(&mut self) -> Result<&mut FenderValue, String> {
        Ok(match self {
            FenderValue::Ref(v) => v.shuffle()?,
            FenderValue::List(v) => {
                v.shuffle(&mut thread_rng());
                self
            }
            e => {
                return Err(format!(
                    "shuffle() can only be called on type `List`, expected `List` found `{:?}`",
                    e.get_type_id()
                ))
            }
        })
    }

    pub fn get_shuffled(&self) -> Result<FenderValue, String> {
        Ok(match self {
            FenderValue::Ref(v) => v.get_shuffled()?,
            FenderValue::List(v) => {
                let mut list = v.clone();
                list.shuffle(&mut thread_rng());
                FenderValue::List(list)
            }
            e => {
                return Err(format!(
                    "shuffle() can only be called on type `List`, expected `List` found `{:?}`",
                    e.get_type_id()
                ))
            }
        })
    }

    pub fn is_empty(&self) -> Result<bool, String> {
        Ok(match self {
            FenderValue::Ref(v) => v.is_empty()?,
            FenderValue::String(s) => s.is_empty(),
            FenderValue::List(v) => v.is_empty(),
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
            FenderValue::Error(e) => format!("Error({e})"),
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
