use std::ops::Deref;

use crate::{
    fender_reference::{FenderReference, InternalReference},
    type_sys::{type_id::FenderTypeId, type_system::FenderTypeSystem},
};
use freight_vm::{function::FunctionRef, value::Value};
use rand::{seq::SliceRandom, thread_rng};

#[derive(Clone, Debug, Default, PartialEq)]
pub enum FenderValue {
    Ref(InternalReference<FenderReference>),
    Int(i64),
    Float(f64),
    Char(char),
    String(InternalReference<String>),
    Bool(bool),
    Error(String),
    Function(FunctionRef<FenderTypeSystem>),
    List(InternalReference<Vec<FenderReference>>),
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
            List(l) => List(l.iter().map(Value::deep_clone).collect::<Vec<_>>().into()),
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

    pub fn push(&mut self, value: FenderReference) -> Result<(), String> {
        match self {
            FenderValue::Ref(v) => v.push(value),
            FenderValue::List(list) => {
                list.push(value);
                Ok(())
            }
            FenderValue::String(s) => Ok(match &*value {
                FenderValue::String(s_b) => s.push_str(&s_b),
                FenderValue::Char(c) => s.push(*c),
                value => s.push_str(value.to_string().as_str()),
            }),
            e => Err(format!(
                "Can only call push on list: Expected type `List` found `{:?}`",
                e.get_type_id()
            )),
        }
    }

    pub fn pop(&mut self) -> Result<FenderReference, String> {
        match self {
            FenderValue::Ref(v) => v.pop(),
            FenderValue::List(list) => Ok(list.pop().unwrap_or_default()),
            FenderValue::String(s) => {
                let len = s.len();
                Ok(FenderValue::Char(s.remove(len - 1)).into())
            }
            e => Err(format!(
                "Can only call pop on list: Expected type `List` found `{:?}`",
                e.get_type_id()
            )),
        }
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

    pub fn remove_at(&mut self, pos: i64) -> Result<FenderReference, String> {
        let list = match self {
            FenderValue::List(l) => l,
            FenderValue::Ref(r) => return r.remove_at(pos),
            e => {
                return Err(format!(
                    "cannot remove from type `{ty}`: expected type `List` found type `{ty}`",
                    ty = e.get_type_id().to_string()
                ))
            }
        };
        let pos = if pos < 0 {
            list.len() as i64 + pos
        } else {
            pos
        };

        if pos < 0 {
            return Err("invalid wrapping index".into());
        }

        let pos = pos as usize;

        if pos >= list.len() {
            return Err(format!(
                "Invalid index: index was {} list length is {}",
                pos,
                list.len()
            ));
        }

        Ok(list.remove(pos))
    }
}

/// Cast functions
impl FenderValue {
    pub fn to_bool(&self) -> FenderValue {
        use FenderValue::*;
        match self {
            FenderValue::Ref(r) => r.to_bool(),
            FenderValue::Int(i) => Bool(*i != 0),
            FenderValue::Float(f) => Bool(*f != 0.0),
            FenderValue::Char(c) => Bool(*c != '\0'),
            FenderValue::String(s) => Bool(!s.is_empty()),
            FenderValue::Bool(b) => Bool(*b),
            FenderValue::Error(_) => Bool(false),
            FenderValue::Function(_) => Bool(true),
            FenderValue::List(l) => Bool(!l.is_empty()),
            FenderValue::Null => Bool(false),
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
            FenderValue::String(s) => s.deref().clone(),
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
