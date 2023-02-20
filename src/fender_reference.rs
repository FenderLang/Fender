use crate::{
    FenderBinaryOperator, FenderTypeId, FenderTypeSystem, FenderUnaryOperator, FenderValue,
};
use freight_vm::{
    expression::Expression,
    function::FunctionRef,
    operators::{binary::BinaryOperator, unary::UnaryOperator},
    value::Value,
};
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
        Self::new((**self).clone())
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

#[derive(Clone, PartialEq)]
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
            FenderValue::Error(_) => &FenderTypeId::Error,
            FenderValue::Null => &FenderTypeId::Null,
            FenderValue::Ref(_) => &FenderTypeId::Reference,
            FenderValue::Function(_) => &FenderTypeId::Function,
        }
    }

    fn deep_clone(&self) -> Self {
        todo!()
    }

    fn dupe_ref(&self) -> Self {
        match self {
            FenderReference::FRef(internal_ref) => FenderReference::FRef(InternalReference(internal_ref.0.clone())),
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

impl BinaryOperator<FenderReference> for FenderBinaryOperator {
    fn apply_2(&self, operand_a: &FenderReference, operand_b: &FenderReference) -> FenderReference {
        use FenderBinaryOperator::*;
        use FenderReference::*;
        use FenderValue::*;

        let value_a = operand_a.deref();
        let value_b = operand_b.deref();

        match (self, value_a, value_b) {
            (Add, Int(a), Int(b)) => FRaw(Int(a + b)),
            (Add, Float(a), Float(b)) => FRaw(Float(a + b)),
            (Add, Int(a), Float(b)) => FRaw(Float(*a as f64 + b)),
            (Add, Float(a), Int(b)) => FRaw(Float(a + *b as f64)),
            (Add, _, _) => FRaw(Error(format!(
                "Cannot add {} and {}",
                operand_a.get_type().to_string(),
                operand_b.get_type().to_string()
            ))),

            (Sub, Int(a), Int(b)) => FRaw(Int(a - b)),
            (Sub, Float(a), Float(b)) => FRaw(Float(a - b)),
            (Sub, Int(a), Float(b)) => FRaw(Float(*a as f64 - b)),
            (Sub, Float(a), Int(b)) => FRaw(Float(a - *b as f64)),
            (Sub, _, _) => FRaw(Error(format!(
                "Cannot subtract {} and {}",
                operand_a.get_type().to_string(),
                operand_b.get_type().to_string()
            ))),

            (Mul, Int(a), Int(b)) => FRaw(Int(a * b)),
            (Mul, Float(a), Float(b)) => FRaw(Float(a * b)),
            (Mul, Int(a), Float(b)) => FRaw(Float(*a as f64 * b)),
            (Mul, Float(a), Int(b)) => FRaw(Float(a * *b as f64)),
            (Mul, _, _) => FRaw(Error(format!(
                "Cannot multiply {} and {}",
                operand_a.get_type().to_string(),
                operand_b.get_type().to_string()
            ))),

            (Div, Int(a), Int(b)) => FRaw(Int(a / b)),
            (Div, Float(a), Float(b)) => FRaw(Float(a / b)),
            (Div, Int(a), Float(b)) => FRaw(Float(*a as f64 / b)),
            (Div, Float(a), Int(b)) => FRaw(Float(a / *b as f64)),
            (Div, _, _) => FRaw(Error(format!(
                "Cannot divide {} and {}",
                operand_a.get_type().to_string(),
                operand_b.get_type().to_string()
            ))),

            (Mod, Int(a), Int(b)) => FRaw(Int(a % b)),
            (Mod, Float(a), Float(b)) => FRaw(Float(a % b)),
            (Mod, Int(a), Float(b)) => FRaw(Float(*a as f64 % b)),
            (Mod, Float(a), Int(b)) => FRaw(Float(a % *b as f64)),
            (Mod, _, _) => FRaw(Error(format!(
                "Cannot modulus {} and {}",
                operand_a.get_type().to_string(),
                operand_b.get_type().to_string()
            ))),

            (Gt, Int(a), Int(b)) => FRaw(Bool(a > b)),
            (Gt, Float(a), Float(b)) => FRaw(Bool(a > b)),
            (Gt, Int(a), Float(b)) => FRaw(Bool(*a as f64 > *b)),
            (Gt, Float(a), Int(b)) => FRaw(Bool(*a > *b as f64)),
            (Gt, _, _) => FRaw(Error(format!(
                "Cannot compare {} and {}",
                operand_a.get_type().to_string(),
                operand_b.get_type().to_string()
            ))),

            (And, Bool(a), Bool(b)) => FRaw(Bool(*a && *b)),
            (And, _, _) => FRaw(Error(format!(
                "Cannot boolean and {} and {}",
                operand_a.get_type().to_string(),
                operand_b.get_type().to_string()
            ))),

            (Or, Bool(a), Bool(b)) => FRaw(Bool(*a || *b)),
            (Or, _, _) => FRaw(Error(format!(
                "Cannot boolean or {} and {}",
                operand_a.get_type().to_string(),
                operand_b.get_type().to_string()
            ))),
        }
    }
}

impl UnaryOperator<FenderReference> for crate::FenderUnaryOperator {
    fn apply_1(&self, operand: &FenderReference) -> FenderReference {
        use FenderReference::*;
        use FenderUnaryOperator::*;
        use FenderValue::*;

        let val = operand.deref();

        match (self, val) {
            (Neg, Int(val)) => FRaw(Int(-val)),
            (Neg, Float(val)) => FRaw(Float(-val)),
            (Neg, _) => FRaw(Error(format!(
                "Cannot negate {}",
                operand.get_type().to_string()
            ))),

            (BoolNeg, Bool(val)) => FRaw(Bool(!val)),
            (BoolNeg, _) => FRaw(Error(format!(
                "Cannot boolean-negate {}",
                operand.get_type().to_string()
            ))),
        }
    }
}
