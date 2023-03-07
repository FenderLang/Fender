use crate::{FenderTypeId, FenderTypeSystem, FenderValue};
use freight_vm::{expression::Expression, function::FunctionRef, value::Value};
use std::{
    cell::UnsafeCell,
    fmt::Debug,
    ops::{Deref, DerefMut},
    rc::Rc,
};

#[derive(Debug)]
pub struct InternalReference(Rc<UnsafeCell<FenderValue>>);

impl Clone for InternalReference {
    fn clone(&self) -> Self {
        // InternalReference(Rc::clone(&self.0))
        InternalReference::new((**self).clone())
    }
}

impl InternalReference {
    pub fn new(value: FenderValue) -> Self {
        Self(Rc::new(UnsafeCell::new(value)))
    }
}

impl Deref for InternalReference {
    type Target = FenderValue;

    fn deref(&self) -> &Self::Target {
        unsafe { &*self.0.get() }
    }
}

impl DerefMut for InternalReference {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.0.get().as_mut().unwrap() }
    }
}

impl PartialEq for InternalReference {
    fn eq(&self, other: &Self) -> bool {
        &**self == &**other
    }
}

#[derive(Clone)]
pub enum FenderReference {
    FRef(InternalReference),
    FRaw(FenderValue),
}

impl FenderReference {
    pub fn get_pass_object(&self) -> FenderReference {
        if self.get_type_id().is_primitive() {
            self.deep_clone()
        } else {
            self.dupe_reference()
        }
    }

    pub fn deep_clone(&self) -> FenderReference {
        // depending on how we use this it should be a `FRaw` instead of `FRef`
        FenderReference::FRef(InternalReference::new(self.deref().deep_clone()))
    }

    pub fn dupe_reference(&self) -> FenderReference {
        match self {
            FenderReference::FRef(_) => self.clone(),
            FenderReference::FRaw(_) => unreachable!(),
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

impl Debug for FenderReference {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        (**self).fmt(f)
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
        }
    }

    fn deep_clone(&self) -> Self {
        todo!()
    }

    fn dupe_ref(&self) -> Self {
        match self {
            FenderReference::FRef(internal_ref) => {
                FenderReference::FRef(InternalReference(internal_ref.0.clone()))
            }
            FenderReference::FRaw(_) => self.clone(),
        }
    }

    fn cast_to_function(&self) -> Option<&FunctionRef<FenderTypeSystem>> {
        match &**self {
            FenderValue::Function(func) => Some(&func),
            _ => None,
        }
    }

    fn uninitialized_reference() -> Self {
        FenderReference::FRef(InternalReference::new(FenderValue::Null))
    }

    fn assign(&mut self, value: FenderReference) {
        *self.deref_mut() = (*value).clone();
    }
}

impl From<FenderReference> for InternalReference {
    fn from(value: FenderReference) -> Self {
        match value {
            FenderReference::FRef(val) => val.clone(),
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
            FenderReference::FRef(val) => &*val,
            FenderReference::FRaw(val) => val,
        }
    }
}
