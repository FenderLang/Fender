#![allow(dead_code)]

use std::{rc::Rc, cell::UnsafeCell};

use freight_vm::{TypeSystem, Value};

struct FenderTypeSystem;

type InternalReference = Rc<UnsafeCell<FenderValue>>;

enum FenderReference {
    Ref(InternalReference),
    Raw(FenderValue),
}

#[derive(Clone, Default)]
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

impl Value for FenderValue {
    type V = FenderTypeSystem;

    fn get_type(&self) -> &<Self::V as crate::TypeSystem>::T {
        match self {
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

impl freight_vm::BinaryOperator<FenderValue> for BinaryOperator {
    fn apply(&self, a: &FenderValue, b: &FenderValue) -> FenderValue {
        use BinaryOperator::*;
        use FenderValue::*;
        match (self, a, b) {
            (Add, Int(a), Int(b)) => Int(a + b),
            (Add, Float(a), Float(b)) => Float(a + b),
            (Add, Int(a), Float(b)) => Float(*a as f64 + b),
            (Add, Float(a), Int(b)) => Float(a + *b as f64),
            (Add, _, _) => Error(format!(
                "Cannot add {} and {}",
                a.get_type().to_string(),
                b.get_type().to_string()
            )),

            (Sub, Int(a), Int(b)) => Int(a - b),
            (Sub, Float(a), Float(b)) => Float(a - b),
            (Sub, Int(a), Float(b)) => Float(*a as f64 - b),
            (Sub, Float(a), Int(b)) => Float(a - *b as f64),
            (Sub, _, _) => Error(format!(
                "Cannot subtract {} and {}",
                a.get_type().to_string(),
                b.get_type().to_string()
            )),

            (Mul, Int(a), Int(b)) => Int(a * b),
            (Mul, Float(a), Float(b)) => Float(a * b),
            (Mul, Int(a), Float(b)) => Float(*a as f64 * b),
            (Mul, Float(a), Int(b)) => Float(a * *b as f64),
            (Mul, _, _) => Error(format!(
                "Cannot multiply {} and {}",
                a.get_type().to_string(),
                b.get_type().to_string()
            )),

            (Div, Int(a), Int(b)) => Int(a / b),
            (Div, Float(a), Float(b)) => Float(a / b),
            (Div, Int(a), Float(b)) => Float(*a as f64 / b),
            (Div, Float(a), Int(b)) => Float(a / *b as f64),
            (Div, _, _) => Error(format!(
                "Cannot divide {} and {}",
                a.get_type().to_string(),
                b.get_type().to_string()
            )),

            (Mod, Int(a), Int(b)) => Int(a % b),
            (Mod, Float(a), Float(b)) => Float(a % b),
            (Mod, Int(a), Float(b)) => Float(*a as f64 % b),
            (Mod, Float(a), Int(b)) => Float(a % *b as f64),
            (Mod, _, _) => Error(format!(
                "Cannot modulus {} and {}",
                a.get_type().to_string(),
                b.get_type().to_string()
            )),

            (And, Bool(a), Bool(b)) => Bool(*a && *b),
            (And, _, _) => Error(format!(
                "Cannot boolean and {} and {}",
                a.get_type().to_string(),
                b.get_type().to_string()
            )),

            (Or, Bool(a), Bool(b)) => Bool(*a || *b),
            (Or, _, _) => Error(format!(
                "Cannot boolean or {} and {}",
                a.get_type().to_string(),
                b.get_type().to_string()
            )),
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

impl freight_vm::UnaryOperator<FenderValue> for UnaryOperator {
    fn apply(&self, val: &FenderValue) -> FenderValue {
        use FenderValue::*;
        use UnaryOperator::*;
        match (self, val) {
            (Neg, Int(val)) => Int(-val),
            (Neg, Float(val)) => Float(-val),
            (Neg, _) => Error(format!("Cannot negate {}", val.get_type().to_string())),

            (BoolNeg, Bool(val)) => Bool(!val),
            (BoolNeg, _) => Error(format!(
                "Cannot boolean-negate {}",
                val.get_type().to_string()
            )),
        }
    }
}

impl TypeSystem for FenderTypeSystem {
    type TypeSystemValue = FenderValue;
    type TypeSystemBinaryOp = BinaryOperator;
    type TypeSystemUnaryOp = UnaryOperator;
    type TypeSystemType = TypeId;
}
