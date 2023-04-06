use self::fender_structs::FenderStruct;
use crate::{
    fender_reference::{FenderReference, InternalReference},
    type_sys::{type_id::FenderTypeId, type_system::FenderTypeSystem},
};
use freight_vm::{function::FunctionRef, value::Value};
use rand::{seq::SliceRandom, thread_rng};
use std::{collections::HashMap, hash::Hash, ops::Deref};
pub mod fender_structs;

#[derive(Clone, Debug, Default)]
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
    Struct(FenderStruct),
    Type(FenderTypeId),
    HashMap(InternalReference<HashMap<FenderValue, InternalReference<FenderReference>>>),
    #[default]
    Null,
}

impl FenderValue {
    pub fn make_error<S: Into<String>>(e_body: S) -> FenderValue {
        FenderValue::Error(e_body.into())
    }
    pub fn make_list(list: Vec<FenderReference>) -> FenderValue {
        FenderValue::List(InternalReference::new(list))
    }
    pub fn make_string(s_body: String) -> FenderValue {
        FenderValue::String(InternalReference::new(s_body))
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
            FenderValue::Struct(_) => FenderTypeId::Struct,
            FenderValue::Type(_) => FenderTypeId::Type,
            FenderValue::HashMap(_) => FenderTypeId::HashMap,
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
            Struct(s) => Struct(FenderStruct {
                struct_id: s.struct_id.clone(),
                data: s
                    .data
                    .iter()
                    .map(|(k, v)| (*k, InternalReference::new(v.deep_clone())))
                    .collect(),
            }),
            Type(t) => Type(t.clone()),
            HashMap(h) => FenderValue::HashMap(InternalReference::new(
                h.iter()
                    .map(|(k, v)| (k.clone(), InternalReference::new(v.deep_clone())))
                    .collect(),
            )),
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
                let mut list = v.deref().clone();
                list.shuffle(&mut thread_rng());
                FenderValue::make_list(list)
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
            FenderValue::String(s) => {
                match &*value {
                    FenderValue::String(s_b) => s.push_str(s_b),
                    FenderValue::Char(c) => s.push(*c),
                    value => s.push_str(value.to_string().as_str()),
                };
                Ok(())
            }
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

    /// Return a raw `FenderValue` unwrapping all references
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

    pub fn insert(
        &mut self,
        key: FenderValue,
        val: FenderReference,
    ) -> Result<FenderReference, String> {
        use FenderValue::*;

        match (self, key) {
            (List(list), Int(index)) => {
                list.insert(index as usize, val);
                Ok(match list.get(index as usize + 1) {
                    Some(v) => v.dupe_ref(),
                    None => Null.into(),
                })
            }
            (HashMap(hash_map), key) => {
                Ok(match hash_map.insert(key, InternalReference::new(val)) {
                    Some(v) => v.dupe_ref(),
                    None => Null.into(),
                })
            }
            (t, _) => Err(format!(
                "Cannot call `Insert` on type `{}`",
                t.get_real_type_id().to_string()
            )),
        }
    }

    pub fn fender_dbg_string(&self) -> String {
        match self {
            FenderValue::Ref(r) => format!("Ref({})", r.fender_dbg_string()),
            v => format!(
                "{}({})",
                v.get_type_id().to_string(),
                v.to_literal_display_string()
            ),
        }
    }
}

/// Cast functions
impl FenderValue {
    pub fn cast_to(&self, typeid: FenderTypeId) -> FenderValue {
        use FenderValue::*;
        match (self, typeid) {
            (Ref(r), t) => r.deref().cast_to(t),
            (v, t) if v.get_type_id() == t => v.clone(),
            (Int(i), FenderTypeId::Float) => Float(*i as f64),
            (Int(i), FenderTypeId::Bool) => Bool(*i != 0),
            (Int(i), FenderTypeId::String) => String(i.to_string().into()),
            (Int(_i), FenderTypeId::Error) => todo!(),
            (Int(i), FenderTypeId::List) => List(vec![Int(*i).into()].into()),
            (Int(i), FenderTypeId::Char) => Char(*i as u8 as char),
            (Float(f), FenderTypeId::Int) => Int(*f as i64),
            (Float(f), FenderTypeId::Bool) => Bool(*f > 0.0),
            (Float(f), FenderTypeId::String) => FenderValue::make_string(f.to_string()),
            (Float(f), FenderTypeId::Error) => FenderValue::make_error(f.to_string()),
            (Float(f), FenderTypeId::List) => FenderValue::make_list(vec![Float(*f).into()]),
            (Float(f), FenderTypeId::Char) => Char(*f as u8 as char),
            (Char(c), FenderTypeId::Int) => Int(*c as i64),
            (Char(c), FenderTypeId::Float) => Float(*c as u64 as f64),
            (Char(c), FenderTypeId::String) => FenderValue::make_string(c.to_string()),
            (Char(c), FenderTypeId::Error) => FenderValue::make_error(c.to_string()),
            (Char(c), FenderTypeId::List) => FenderValue::make_list(vec![Char(*c).into()]),
            (String(s), FenderTypeId::Int) => match s.parse() {
                Ok(i) => Int(i),
                _ => FenderValue::make_error(format!("Invalid int string: {}", s.deref())),
            },
            (String(s), FenderTypeId::Float) => match s.parse() {
                Ok(i) => Float(i),
                _ => FenderValue::make_error(format!("Invalid int string: {}", s.deref())),
            },
            (String(s), FenderTypeId::Bool) => Bool(s.to_lowercase() == "true"),
            (String(s), FenderTypeId::Error) => Error(s.deref().into()),
            (String(s), FenderTypeId::List) => {
                FenderValue::make_list(s.chars().map(|c| Char(c).into()).collect::<Vec<_>>())
            }
            (String(s), FenderTypeId::Char) => {
                if s.len() > 1 {
                    Char(s.chars().next().unwrap())
                } else {
                    Char('\0')
                }
            }
            (Bool(b), FenderTypeId::Int) => Int(*b as i64),
            (Bool(b), FenderTypeId::Float) => Float(if *b { 1.0 } else { 0.0 }),
            (Bool(b), FenderTypeId::String) => FenderValue::make_string(b.to_string()),
            (Bool(b), FenderTypeId::Char) => Char(if *b { 't' } else { 'f' }),
            (Error(e), FenderTypeId::String) => FenderValue::make_string(e.into()),
            (Null, FenderTypeId::Int) => Int(Default::default()),
            (Null, FenderTypeId::Float) => Float(Default::default()),
            (Null, FenderTypeId::Bool) => Bool(Default::default()),
            (Null, FenderTypeId::String) => FenderValue::make_string("NULL".into()),
            (Null, FenderTypeId::Error) => Error("is NULL".into()),
            (Null, FenderTypeId::List) => FenderValue::make_list(Vec::new()),
            (Null, FenderTypeId::Char) => Char('\0'),
            (v, t) => FenderValue::make_error(format!(
                "Cannot cast from type `{}` to `{}`",
                v.get_type_id().to_string(),
                t.to_string()
            )),
        }
    }

    pub fn to_bool(&self) -> FenderValue {
        use FenderValue::*;
        match self {
            Ref(r) => r.to_bool(),
            Int(i) => Bool(*i != 0),
            Float(f) => Bool(*f != 0.0),
            Char(c) => Bool(*c != '\0'),
            String(s) => Bool(!s.is_empty()),
            Bool(b) => Bool(*b),
            Error(_) => Bool(false),
            Function(_) => Bool(true),
            List(l) => Bool(!l.is_empty()),
            Struct(_) => Bool(false),
            Null => Bool(false),
            Type(_) => Bool(true),
            HashMap(h) => Bool(!h.is_empty()),
        }
    }

    pub fn join_to_string(&self) -> FenderValue {
        match self {
            FenderValue::Ref(r) => r.join_to_string(),
            FenderValue::String(_) => self.clone(),
            FenderValue::List(l) => {
                FenderValue::make_string(l.iter().map(|i| i.to_string()).collect::<String>())
            }
            e => FenderValue::make_error(format!(
                "Cannot join type `{}` to String",
                e.get_type_id().to_string()
            )),
        }
    }

    pub fn to_literal_display_string(&self) -> String {
        match self {
            FenderValue::Ref(v) => format!("Ref({})", v.to_literal_display_string()),
            FenderValue::Char(c) => format!("'{c}'"),
            FenderValue::String(s) => format!("{:?}", s.deref()),
            v => v.to_string(),
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
            FenderValue::Error(e) => format!("Error({e:?})"),
            FenderValue::Function(_) => "Function".to_string(),
            FenderValue::Null => "null".to_string(),
            FenderValue::List(list) => format!(
                "[{}]",
                list.iter()
                    .map(|e| e.to_literal_display_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            FenderValue::Struct(s) => s.to_string(),
            FenderValue::Type(t) => format!("Type({})", t.to_string()),
            FenderValue::HashMap(h) => format!(
                "[{}]",
                h.iter()
                    .map(|(k, v)| format!(
                        "{}:{}",
                        k.to_literal_display_string(),
                        v.to_literal_display_string()
                    ))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }
}

impl PartialEq for FenderValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Ref(l0), Self::Ref(r0)) => l0 == r0,
            (Self::Int(l0), Self::Int(r0)) => l0 == r0,
            (Self::Float(l0), Self::Float(r0)) => l0 == r0,
            (Self::Char(l0), Self::Char(r0)) => l0 == r0,
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            (Self::Bool(l0), Self::Bool(r0)) => l0 == r0,
            (Self::Error(l0), Self::Error(r0)) => l0 == r0,
            (Self::Function(l0), Self::Function(r0)) => l0 == r0,
            (Self::List(l0), Self::List(r0)) => l0 == r0,
            (Self::Struct(l0), Self::Struct(r0)) => l0 == r0,
            (Self::Type(l0), Self::Type(r0)) => l0 == r0,
            (Self::HashMap(l0), Self::HashMap(r0)) => l0.iter().eq(r0.iter()),
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl Eq for FenderValue {}

impl Hash for FenderValue {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
    }
}
