use crate::{
    fender_reference::{FenderReference, InternalReference},
    fender_value::{fender_structs::FenderStruct, FenderValue},
    type_sys::{type_id::FenderTypeId, type_system::FenderTypeSystem},
};
use freight_vm::{
    execution_engine::ExecutionEngine,
    operators::{BinaryOperator, Initializer, UnaryOperator},
    value::Value,
};
use std::{collections::HashMap, ops::Deref};

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
    Index,
    FieldAccess,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FenderUnaryOperator {
    Neg,
    BoolNeg,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FenderInitializer {
    List,
    String,
    Struct(usize),
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

num_op_func!(num_add, +);
num_op_func!(num_sub, -);
num_op_func!(num_mul, *);
num_op_func!(num_div, /);
num_op_func!(num_rem, %);
cmp_op_func!(num_gt, >);
cmp_op_func!(num_ge, >=);
cmp_op_func!(num_le, <=);
cmp_op_func!(num_lt, <);
cmp_op_func!(num_eq, ==);
cmp_op_func!(num_ne, !=);
bool_op_func!(num_or, ||);
bool_op_func!(num_and, &&);

fn eq(a: &FenderReference, b: &FenderReference) -> FenderReference {
    let a_val: &FenderValue = a.into();
    let b_val: &FenderValue = b.into();
    match (a_val, b_val) {
        (FenderValue::Ref(a), FenderValue::Ref(b)) => eq(a.deref(), b.deref()),
        (FenderValue::Ref(r), _) => eq(r.deref(), b),
        (_, FenderValue::Ref(r)) => eq(a, r.deref()),
        (FenderValue::String(a), FenderValue::String(b)) => FenderValue::Bool(a == b).into(),
        (FenderValue::Char(a), FenderValue::Char(b)) => FenderValue::Bool(a == b).into(),
        (FenderValue::Char(c), FenderValue::String(s))
        | (FenderValue::String(s), FenderValue::Char(c)) => {
            FenderValue::Bool(s.len() == 1 && s.deref().chars().next().unwrap() == *c).into()
        }
        (FenderValue::Int(_), FenderValue::Int(_))
        | (FenderValue::Float(_), FenderValue::Float(_))
        | (FenderValue::Int(_), FenderValue::Float(_))
        | (FenderValue::Float(_), FenderValue::Int(_)) => num_eq(a, b),
        (a, b) if a.get_real_type_id() != b.get_real_type_id() => FenderValue::Bool(false).into(),
        _ => FenderValue::make_error(format!(
            "cannot run `eq` on {} and {}",
            a.get_real_type_id().to_string(),
            b.get_real_type_id().to_string()
        ))
        .into(),
    }
}

fn ne(a: &FenderReference, b: &FenderReference) -> FenderReference {
    let a_val: &FenderValue = a.into();
    let b_val: &FenderValue = b.into();

    match (a_val, b_val) {
        (FenderValue::Ref(a), FenderValue::Ref(b)) => ne(a.deref(), b.deref()),
        (FenderValue::Ref(r), _) => ne(r.deref(), b),
        (_, FenderValue::Ref(r)) => ne(a, r.deref()),
        (FenderValue::String(a), FenderValue::String(b)) => FenderValue::Bool(a != b).into(),
        (FenderValue::Char(a), FenderValue::Char(b)) => FenderValue::Bool(a != b).into(),
        (FenderValue::Char(c), FenderValue::String(s))
        | (FenderValue::String(s), FenderValue::Char(c)) => {
            FenderValue::Bool(s.len() != 1 || s.deref().chars().next().unwrap() != *c).into()
        }

        (FenderValue::Int(_), FenderValue::Int(_))
        | (FenderValue::Float(_), FenderValue::Float(_))
        | (FenderValue::Int(_), FenderValue::Float(_))
        | (FenderValue::Float(_), FenderValue::Int(_)) => num_ne(a, b),
        (a, b) if a.get_real_type_id() != b.get_real_type_id() => FenderValue::Bool(true).into(),
        _ => FenderValue::make_error(format!(
            "cannot run `ne` on {} and {}",
            a.get_real_type_id().to_string(),
            b.get_real_type_id().to_string()
        ))
        .into(),
    }
}

fn add(a: &FenderReference, b: &FenderReference) -> FenderReference {
    let a_val: &FenderValue = a.into();
    let b_val: &FenderValue = b.into();
    match (a_val, b_val) {
        (FenderValue::String(s), other) | (other, FenderValue::String(s)) => {
            FenderValue::String(format!("{}{}", s.deref(), other.to_string()).into()).into()
        }
        _ => num_add(a, b),
    }
}

fn index_op(a: &FenderReference, b: &FenderReference) -> FenderReference {
    let FenderValue::Int(pos) =  b.deref() else {
                    return FenderReference::FRaw(FenderValue::Error(format!(
                        "cannot index with `{}` type",
                        b.get_real_type_id().to_string()
                    )));
                };

    let len = match a.deref() {
        FenderValue::String(s) => s.len(),
        FenderValue::List(l) => l.len(),
        _ => {
            return FenderValue::Error(format!(
                "cannot index with `{}` type",
                a.get_real_type_id().to_string()
            ))
            .into();
        }
    };

    let pos = if pos.is_negative() {
        len - pos.unsigned_abs() as usize
    } else {
        pos.unsigned_abs() as usize
    };

    if pos >= len {
        return FenderValue::Error(format!("trying to get index {pos} but length is {len}")).into();
    }

    match a.deref() {
        FenderValue::String(s) => FenderValue::Char(s.chars().nth(pos).unwrap()).into(),
        FenderValue::List(l) => l[pos].dupe_ref(),
        _ => unreachable!(),
    }
}

fn field_access(a: &FenderReference, b: &FenderReference) -> FenderReference {
    match (&**a, &**b) {
        (FenderValue::Struct(s), FenderValue::Int(i)) => s.data[i].dupe_ref(),
        e => unreachable!("{:?}", e),
    }
}

impl BinaryOperator<FenderReference> for FenderBinaryOperator {
    fn apply_2(&self, a: &FenderReference, b: &FenderReference) -> FenderReference {
        use FenderBinaryOperator::*;
        match self {
            Add => add(a, b),
            Sub => num_sub(a, b),
            Mul => num_mul(a, b),
            Div => num_div(a, b),
            Mod => num_rem(a, b),
            Gt => num_gt(a, b),
            Lt => num_lt(a, b),
            Le => num_le(a, b),
            Ge => num_ge(a, b),
            Eq => eq(a, b),
            Ne => ne(a, b),
            And => num_and(a, b),
            Or => num_or(a, b),
            Index => index_op(a, b),
            FieldAccess => field_access(a, b),
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

impl Initializer<FenderTypeSystem> for FenderInitializer {
    fn initialize(
        &self,
        values: Vec<FenderReference>,
        ctx: &mut ExecutionEngine<FenderTypeSystem>,
    ) -> FenderReference {
        match self {
            FenderInitializer::List => FenderValue::List(
                values
                    .into_iter()
                    .map(|val| FenderReference::FRef(InternalReference::from(val)))
                    .collect::<Vec<_>>()
                    .into(),
            )
            .into(),
            FenderInitializer::String => {
                let mut collected = String::new();
                for v in values {
                    if let FenderValue::String(s) = &*v {
                        collected.push_str(s);
                    } else {
                        collected.push_str(&v.to_string());
                    }
                }
                FenderValue::String(collected.into()).into()
            }
            FenderInitializer::Struct(id) => {
                let id = ctx.context.struct_table.type_list().get(*id).cloned();
                match id {
                    Some(struct_type) => {
                        let mut ret = FenderStruct {
                            struct_id: struct_type.clone(),
                            data: HashMap::new(),
                        };
                        for (key, type_id, val) in
                            struct_type.fields.iter().zip(values.into_iter()).map(
                                |((name, type_id), val)| {
                                    (
                                        ctx.context.struct_table.field_index(name) as i64,
                                        type_id,
                                        val.into_ref(),
                                    )
                                },
                            )
                        {
                            match type_id {
                                Some(type_id)
                                    if *type_id != val.get_type_id()
                                        && val.get_type_id() != FenderTypeId::Null =>
                                {
                                    return FenderValue::make_error(format!(
                                        "Incorrect type used: expected `{}` found `{}`",
                                        type_id.to_string(),
                                        val.get_type_id().to_string()
                                    ))
                                    .into();
                                }
                                _ => (),
                            }

                            ret.data.insert(key, InternalReference::from(val));
                        }
                        FenderValue::Struct(ret).into()
                    }
                    None => todo!(),
                }
            }
        }
    }
}
