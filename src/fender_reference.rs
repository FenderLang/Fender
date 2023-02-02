use crate::{FenderBinaryOperator, FenderTypeSystem, FenderUnaryOperator, FenderValue, TypeId};
use freight_vm::Value;
use std::{cell::UnsafeCell, rc::Rc};

pub type InternalReference = Rc<UnsafeCell<FenderValue>>;

#[derive(Clone, Debug)]
pub enum FenderReference {
    FRef(InternalReference),
    FRaw(FenderValue),
}

impl FenderReference {
    fn assign(&self, val: FenderReference) {
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
}

impl Default for FenderReference {
    fn default() -> Self {
        FenderReference::FRaw(FenderValue::Null)
    }
}

impl Value for FenderReference {
    type V = FenderTypeSystem;

    fn get_type(&self) -> &<Self::V as crate::TypeSystem>::T {
        let val = match self {
            FenderReference::FRaw(v) => v,
            FenderReference::FRef(v) => unsafe { v.get().as_ref().unwrap() },
        };
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

        let value_a = match operand_a {
            FenderReference::FRef(v) => unsafe { v.get().as_ref().unwrap() },
            FenderReference::FRaw(v) => v,
        };
        let value_b = match operand_b {
            FenderReference::FRef(v) => unsafe { v.get().as_ref().unwrap() },
            FenderReference::FRaw(v) => v,
        };

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

        let val = match operand {
            FenderReference::FRef(v) => unsafe { v.get().as_ref().unwrap() },
            FenderReference::FRaw(v) => v,
        };

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
