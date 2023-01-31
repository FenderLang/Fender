#![allow(dead_code)]

use freight_vm::{TypeSystem, Value};
use std::{cell::UnsafeCell, rc::Rc};

struct FenderTypeSystem;

type InternalReference = Rc<UnsafeCell<FenderValue>>;

#[derive(Clone, Debug)]
enum FenderReference {
    FRef(InternalReference),
    FRaw(FenderValue),
}

impl Default for FenderReference {
    fn default() -> Self {
        FenderReference::FRaw(FenderValue::Null)
    }
}

#[derive(Clone, Debug, Default)]
enum FenderValue {
    Ref(InternalReference),
    Int(i64),
    Float(f64),
    Bool(bool),
    Error(String),
    #[default]
    Null,
}

enum TypeId {
    Int,
    Float,
    Bool,
    Error,
    Null,
    Reference,
}

enum BinaryOperator {
    Add,
    Sub,
    Div,
    Mod,
    Mul,
    And,
    Or,
}

enum UnaryOperator {
    Neg,
    BoolNeg,
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

impl ToString for TypeId {
    fn to_string(&self) -> String {
        match self {
            TypeId::Int => "Int",
            TypeId::Float => "Float",
            TypeId::Bool => "Bool",
            TypeId::Error => "Error",
            TypeId::Null => "Null",
            TypeId::Reference => "Reference",
        }
        .to_owned()
    }
}

impl freight_vm::BinaryOperator<FenderReference> for BinaryOperator {
    fn apply(&self, operand_a: &FenderReference, operand_b: &FenderReference) -> FenderReference {
        use BinaryOperator::*;
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
            BinaryOperator::Add => 0,
            BinaryOperator::Sub => 0,
            BinaryOperator::Div => 1,
            BinaryOperator::Mod => 1,
            BinaryOperator::Mul => 1,
            BinaryOperator::And => 2,
            BinaryOperator::Or => 2,
        }
    }
}

impl freight_vm::UnaryOperator<FenderReference> for UnaryOperator {
    fn apply(&self, operand: &FenderReference) -> FenderReference {
        use FenderReference::*;
        use FenderValue::*;
        use UnaryOperator::*;

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

impl TypeSystem for FenderTypeSystem {
    type V = FenderReference;
    type B = BinaryOperator;
    type U = UnaryOperator;
    type T = TypeId;
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
