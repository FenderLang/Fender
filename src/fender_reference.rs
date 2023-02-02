use crate::{FenderBinaryOperator, FenderTypeSystem, FenderUnaryOperator, FenderValue, TypeId};
use freight_vm::Value;
use std::{
    cell::UnsafeCell,
    ops::{Deref, DerefMut},
    rc::Rc,
};

pub type InternalReference = Rc<UnsafeCell<FenderValue>>;

#[derive(Clone, Debug)]
pub enum FenderReference {
    FRef(InternalReference),
    FRaw(FenderValue),
}

impl FenderReference {
    pub fn assign(&self, val: FenderReference) {
        use FenderReference::*;

        let val = match val {
            FRef(ref_val) => unsafe { ref_val.get().as_ref().unwrap().clone() },
            FRaw(val) => val,
        };

        match self {
            FRef(v) => unsafe {
                *v.get() = val;
            },
            FRaw(_) => unreachable!(),
        };
    }

    pub fn get_pass_object(&self) -> FenderReference {
        if self.get_type_id().is_primitive() {
            self.get_value_pass()
        } else {
            self.get_reference_pass()
        }
    }
    pub fn get_value_pass(&self) -> FenderReference {
        // depending on how we use this it should be a `FRaw` instead of `FRef`
        FenderReference::FRef(Rc::new(UnsafeCell::new(self.deref().deep_clone())))
    }
    pub fn get_reference_pass(&self) -> FenderReference {
        match self {
            FenderReference::FRef(inner_rc) => FenderReference::FRef(inner_rc.clone()),

            // either want this to make a reference to something that used to be raw or we just want this to be unreachable
            //
            // FenderReference::FRaw(_) => unreachable!()
            // FenderReference::FRaw(val) => FenderReference::FRef(Rc::new(UnsafeCell::new(val))),
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
            FenderReference::FRef(v) => unsafe { v.get().as_ref().unwrap() },
        }
    }
}

impl DerefMut for FenderReference {
    fn deref_mut(&mut self) -> &mut Self::Target {
        match self {
            FenderReference::FRaw(v) => v,
            FenderReference::FRef(v) => unsafe {
                let t = &mut *v.get();
                todo!()
            },
        }
    }
}

impl Value for FenderReference {
    type V = FenderTypeSystem;

    fn get_type(&self) -> &<Self::V as crate::TypeSystem>::T {
        let val = self.deref();
        match val {
            FenderValue::Int(_) => &TypeId::Int,
            FenderValue::Float(_) => &TypeId::Float,
            FenderValue::Bool(_) => &TypeId::Bool,
            FenderValue::Error(_) => &TypeId::Error,
            FenderValue::Null => &TypeId::Null,
            FenderValue::Ref(_) => &TypeId::Reference,
        }
    }
}

impl freight_vm::BinaryOperator<FenderReference> for FenderBinaryOperator {
    fn apply(&self, operand_a: &FenderReference, operand_b: &FenderReference) -> FenderReference {
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

    fn priority(&self) -> usize {
        match self {
            FenderBinaryOperator::Add => 0,
            FenderBinaryOperator::Sub => 0,
            FenderBinaryOperator::Div => 1,
            FenderBinaryOperator::Mod => 1,
            FenderBinaryOperator::Mul => 1,
            FenderBinaryOperator::And => 2,
            FenderBinaryOperator::Or => 2,
        }
    }
}

impl freight_vm::UnaryOperator<FenderReference> for crate::FenderUnaryOperator {
    fn apply(&self, operand: &FenderReference) -> FenderReference {
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
