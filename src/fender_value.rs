use crate::{fender_reference::InternalReference, TypeId};

#[derive(Clone, Debug, Default)]
pub enum FenderValue {
    Ref(InternalReference),
    Int(i64),
    Float(f64),
    Bool(bool),
    Error(String),
    #[default]
    Null,
}

impl FenderValue {
    pub fn get_type_id(&self) -> TypeId {
        match self {
            FenderValue::Ref(_) => TypeId::Reference,
            FenderValue::Int(_) => TypeId::Int,
            FenderValue::Float(_) => TypeId::Float,
            FenderValue::Bool(_) => TypeId::Bool,
            FenderValue::Error(_) => TypeId::Error,
            FenderValue::Null => TypeId::Null,
        }
    }
    pub fn get_real_type_id(&self) -> TypeId {
        match self {
            FenderValue::Ref(val) => unsafe { val.get().as_ref().unwrap().get_real_type_id() },
            _ => self.get_type_id(),
        }
    }

    pub fn deep_clone(&self)-> FenderValue{
        todo!()
    }
}
