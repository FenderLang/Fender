use std::ops::Deref;

use freight_vm::{
    operators::{binary::BinaryOperator, unary::UnaryOperator},
    value::Value,
};

use crate::{FenderReference, FenderValue};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FenderBinaryOperator {
    Add,
    Sub,
    Div,
    Mod,
    Mul,
    And,
    Or,
    Gt,
    Lt,
    Ge,
    Le,
    Eq,
    Ne,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FenderUnaryOperator {
    Neg,
    BoolNeg,
}

macro_rules! num_op_func {
    ($name:ident, $op:tt) => {
        fn $name(a: &FenderReference, b: &FenderReference) -> FenderReference {
            use FenderReference::*;
            use FenderValue::*;
            match (a.deref(), b.deref()) {
                (Int(a), Int(b)) => FRaw(Int(a $op b)),
                (Float(a), Float(b)) => FRaw(Float(a $op b)),
                (Int(a), Float(b)) => FRaw(Float(*a as f64 $op b)),
                (Float(a), Int(b)) => FRaw(Float(a $op *b as f64)),
                (a, b) => FRaw(Error(format!(
                    "Cannot {} {} and {}",
                    stringify!($name),
                    a.get_real_type_id().to_string(),
                    b.get_real_type_id().to_string()
                ))),
            }
        }
    };
}

macro_rules! cmp_op_func {
    ($name:ident, $op:tt) => {
        fn $name(a: &FenderReference, b: &FenderReference) -> FenderReference {
            use FenderReference::*;
            use FenderValue::*;
            match (a.deref(), b.deref()) {
                (Int(a), Int(b)) => FRaw(Bool(a $op b)),
                (Float(a), Float(b)) => FRaw(Bool(a $op b)),
                (Int(a), Float(b)) => FRaw(Bool((*a as f64) $op *b)),
                (Float(a), Int(b)) => FRaw(Bool(a $op &(*b as f64))),
                (a, b) => FRaw(Error(format!(
                        "Cannot {} {} and {}",
                    stringify!($name),
                    a.get_real_type_id().to_string(),
                    b.get_real_type_id().to_string()
                ))),
            }
        }
    };
}

macro_rules! bool_op_func {
    ($name:ident, $op:tt) => {
        fn $name(a: &FenderReference, b: &FenderReference) -> FenderReference {
            use FenderReference::*;
            use FenderValue::*;
            match (a.deref(), b.deref()) {
                (Bool(a), Bool(b)) => FRaw(Bool(*a $op *b)),
                (a, b) => FRaw(Error(format!(
                        "Cannot {} {} and {}",
                    stringify!($name),
                    a.get_real_type_id().to_string(),
                    b.get_real_type_id().to_string()
                ))),
            }
        }
    };
}

num_op_func!(add, +);
num_op_func!(sub, -);
num_op_func!(mul, *);
num_op_func!(div, /);
num_op_func!(rem, %);
cmp_op_func!(gt, >);
cmp_op_func!(ge, >=);
cmp_op_func!(le, <=);
cmp_op_func!(lt, <);
cmp_op_func!(eq, ==);
cmp_op_func!(ne, !=);
bool_op_func!(or, ||);
bool_op_func!(and, &&);

impl BinaryOperator<FenderReference> for FenderBinaryOperator {
    fn apply_2(&self, a: &FenderReference, b: &FenderReference) -> FenderReference {
        use FenderBinaryOperator::*;

        match self {
            Add => add(a, b),
            Sub => sub(a, b),
            Mul => mul(a, b),
            Div => div(a, b),
            Mod => rem(a, b),
            Gt => gt(a, b),
            Lt => lt(a, b),
            Le => le(a, b),
            Ge => ge(a, b),
            Eq => eq(a, b),
            Ne => ne(a, b),
            And => and(a, b),
            Or => or(a, b),
        }
    }
}

impl UnaryOperator<FenderReference> for FenderUnaryOperator {
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
