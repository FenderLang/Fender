use crate::{
    fender_value::FenderValue,
    type_sys::{type_id::FenderTypeId, type_system::FenderTypeSystem},
};
use freight_vm::{expression::Expression, function::FunctionRef, value::Value};
use std::{
    cell::UnsafeCell,
    fmt::Debug,
    ops::{Deref, DerefMut},
    rc::Rc,
};

#[derive(Clone)]
pub struct InternalReference<T>(Rc<UnsafeCell<T>>);

impl<T: Debug> Debug for InternalReference<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "InternalReference({:?})", **self)
    }
}

impl<T> InternalReference<T> {
    pub fn new(value: T) -> Self {
        Self(Rc::new(UnsafeCell::new(value)))
    }
}

impl<T> Deref for InternalReference<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { &*self.0.get() }
    }
}

impl<T> DerefMut for InternalReference<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.0.get().as_mut().unwrap() }
    }
}

impl<T: PartialEq> PartialEq for InternalReference<T> {
    fn eq(&self, other: &Self) -> bool {
        **self == **other
    }
}

impl<T: PartialOrd> PartialOrd for InternalReference<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        (**self).partial_cmp(&**other)
    }
}

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

impl<T> From<T> for InternalReference<T> {
    fn from(value: T) -> Self {
        InternalReference::new(value)
    }
}

impl From<FenderValue> for Expression<FenderTypeSystem> {
    fn from(value: FenderValue) -> Self {
        Expression::RawValue(value.into())
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

    fn get_type(&self) -> &<Self::TS as crate::TypeSystem>::TypeId {
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
            FenderValue::Range(_,_,_) => &FenderTypeId::Range,
        }
    }

    fn deep_clone(&self) -> Self {
        match self {
            FenderReference::FRaw(v) => FenderReference::FRaw(v.deep_clone()),
            FenderReference::FRef(r) => {
                FenderReference::FRef(InternalReference::new(r.deep_clone()))
            }
        }
    }

    fn dupe_ref(&self) -> Self {
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
        FenderReference::FRaw(FenderValue::List(InternalReference::new(values)))
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
