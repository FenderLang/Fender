use self::internal_reference::InternalReference;
use crate::type_sys::{
    fender_value::FenderValue, freight_type_system::FenderTypeSystem, type_id::FenderTypeId,
};
use freight_vm::{function::FunctionRef, value::Value};
use std::{
    fmt::Debug,
    ops::{Deref, DerefMut},
};

pub mod internal_reference;

#[derive(Debug)]
pub enum FenderReference {
    FRef(InternalReference<FenderValue>),
    FRaw(FenderValue),
}

impl FenderReference {
    pub fn get_pass_object(&self) -> FenderReference {
        match self {
            FenderReference::FRef(r) => FenderReference::FRef(r.deref().clone().into()),
            FenderReference::FRaw(v) => FenderReference::FRaw(v.clone()),
        }
    }
}

impl PartialEq for FenderReference {
    fn eq(&self, other: &Self) -> bool {
        **self == **other
    }
}

impl Default for FenderReference {
    fn default() -> Self {
        FenderReference::FRaw(FenderValue::Null)
    }
}

impl Deref for FenderReference {
    type Target = FenderValue;

    fn deref(&self) -> &Self::Target {
        match self {
            FenderReference::FRaw(v) => v,
            FenderReference::FRef(v) => v,
        }
    }
}

impl DerefMut for FenderReference {
    fn deref_mut(&mut self) -> &mut Self::Target {
        match self {
            FenderReference::FRaw(v) => v,
            FenderReference::FRef(v) => v,
        }
    }
}

impl From<FenderValue> for FenderReference {
    fn from(value: FenderValue) -> Self {
        FenderReference::FRaw(value)
    }
}

impl From<FunctionRef<FenderTypeSystem>> for FenderReference {
    fn from(value: FunctionRef<FenderTypeSystem>) -> Self {
        FenderReference::FRaw(FenderValue::Function(value))
    }
}

impl Clone for FenderReference {
    fn clone(&self) -> Self {
        self.get_pass_object()
    }
}

impl Value for FenderReference {
    type TS = FenderTypeSystem;

    fn get_type(&self) -> &<Self::TS as freight_vm::TypeSystem>::TypeId {
        let val = self.deref();
        match val {
            FenderValue::Int(_) => &FenderTypeId::Int,
            FenderValue::Float(_) => &FenderTypeId::Float,
            FenderValue::Bool(_) => &FenderTypeId::Bool,
            FenderValue::Char(_) => &FenderTypeId::Char,
            FenderValue::Error(_) => &FenderTypeId::Error,
            FenderValue::Null => &FenderTypeId::Null,
            FenderValue::Ref(_) => &FenderTypeId::Reference,
            FenderValue::Function(_) => &FenderTypeId::Function,
            FenderValue::String(_) => &FenderTypeId::String,
            FenderValue::List(_) => &FenderTypeId::List,
            FenderValue::Struct(_) => &FenderTypeId::Struct,
            FenderValue::Type(_) => &FenderTypeId::Type,
            FenderValue::HashMap(_) => &FenderTypeId::HashMap,
            FenderValue::Iterator(_) => &FenderTypeId::Iterator,
        }
    }

    fn deep_clone(&self) -> FenderReference {
        match self {
            FenderReference::FRaw(v) => FenderReference::FRaw(v.deep_clone()),
            FenderReference::FRef(r) => {
                FenderReference::FRef(InternalReference::new(r.deep_clone()))
            }
        }
    }

    fn dupe_ref(&self) -> FenderReference {
        match self {
            FenderReference::FRef(internal_ref) => FenderReference::FRef(internal_ref.clone()),
            FenderReference::FRaw(_) => self.deep_clone(),
        }
    }

    fn cast_to_function(&self) -> Option<&FunctionRef<FenderTypeSystem>> {
        match &**self {
            FenderValue::Function(func) => Some(func),
            _ => None,
        }
    }

    fn uninitialized_reference() -> Self {
        FenderReference::FRef(InternalReference::new(FenderValue::Null))
    }

    fn assign(&mut self, value: FenderReference) {
        match &mut *self.deref_mut() {
            FenderValue::Ref(v) => v.assign(value),
            val => *val = (*value).clone(),
        }
    }

    fn into_ref(self) -> Self {
        match self {
            FenderReference::FRef(_) => self,
            FenderReference::FRaw(v) => FenderReference::FRef(InternalReference::new(v)),
        }
    }

    fn gen_list(values: Vec<Self>) -> Self {
        FenderReference::FRaw(FenderValue::List(InternalReference::new(
            values.into_iter().collect(),
        )))
    }
}

impl From<FenderReference> for InternalReference<FenderValue> {
    fn from(value: FenderReference) -> Self {
        match value {
            FenderReference::FRef(val) => val,
            FenderReference::FRaw(val) => InternalReference::new(val),
        }
    }
}

impl From<FenderReference> for FenderValue {
    fn from(value: FenderReference) -> Self {
        match value {
            FenderReference::FRef(val) => val.deref().clone(),
            FenderReference::FRaw(val) => val,
        }
    }
}

impl<'a> From<&'a FenderReference> for &'a FenderValue {
    fn from(value: &'a FenderReference) -> Self {
        match value {
            FenderReference::FRef(val) => val,
            FenderReference::FRaw(val) => val,
        }
    }
}
