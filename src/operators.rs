use crate::type_sys::{
    fender_reference::{internal_reference::InternalReference, FenderReference},
    fender_value::{fender_structs::FenderStruct, FenderValue},
    freight_type_system::FenderTypeSystem,
    type_id::FenderTypeId,
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
    AssignOperate(Box<FenderBinaryOperator>),
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
    HashMap,
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
        (FenderValue::Null, FenderValue::Null) => FenderValue::Bool(true).into(),
        (FenderValue::List(a_list), FenderValue::List(b_list)) => {
            if a_list.len() != b_list.len() {
                FenderValue::Bool(false).into()
            } else {
                FenderValue::Bool(a_list.iter().eq(b_list.iter())).into()
            }
        }
        (a, b) if a.get_real_type_id() != b.get_real_type_id() => FenderValue::Bool(false).into(),
        (a, b) => FenderValue::Bool(a == b).into(),
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
        (FenderValue::Null, FenderValue::Null) => FenderValue::Bool(false).into(),
        (FenderValue::List(a_list), FenderValue::List(b_list)) => {
            if a_list.len() != b_list.len() {
                FenderValue::Bool(true).into()
            } else {
                FenderValue::Bool(a_list.iter().ne(b_list.iter())).into()
            }
        }
        (a, b) if a.get_real_type_id() != b.get_real_type_id() => FenderValue::Bool(true).into(),
        (a, b) => FenderValue::Bool(a != b).into(),
    }
}

fn add(a: &FenderReference, b: &FenderReference) -> FenderReference {
    let a_val: &FenderValue = a.into();
    let b_val: &FenderValue = b.into();
    match (a_val, b_val) {
        (FenderValue::String(s), other) => {
            FenderValue::String(format!("{}{}", s.deref(), other.to_string()).into()).into()
        }
        (other, FenderValue::String(s)) => {
            FenderValue::String(format!("{}{}", other.to_string(), s.deref()).into()).into()
        }
        _ => num_add(a, b),
    }
}

fn index_hash_map(a: &FenderReference, b: &FenderReference) -> FenderReference {
    let FenderValue::HashMap(hash_map) = a.deref() else {
        unreachable!()
    };

    match hash_map.get(b) {
        Some(v) => v.dupe_ref(),
        None => FenderValue::Null.into(),
    }
}

fn index_op(a: &FenderReference, b: &FenderReference) -> FenderReference {
    if let FenderValue::HashMap(_) = a.deref() {
        return index_hash_map(a, b);
    }

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
            AssignOperate(op) => {
                let result = op.apply_2(a, b);
                a.dupe_ref().assign(result);
                Default::default()
            }
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
        mut values: Vec<FenderReference>,
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
            FenderInitializer::HashMap => {
                let mut hash_map = HashMap::new();

                while !values.is_empty() {
                    let val = values.pop().unwrap();
                    let key = values.pop().unwrap();
                    hash_map.insert(key.unwrap_value(), val.into_ref());
                }

                FenderValue::HashMap(hash_map.into()).into()
            }
            FenderInitializer::Struct(id) => self.initialize_struct(values, ctx, *id),
        }
    }
}

impl FenderInitializer {
    fn initialize_struct(
        &self,
        values: Vec<FenderReference>,
        ctx: &mut ExecutionEngine<FenderTypeSystem>,
        id: usize,
    ) -> FenderReference {
        match ctx.context.struct_table.type_list().get(id).cloned() {
            Some(struct_type) => {
                let mut ret = FenderStruct {
                    struct_id: struct_type.clone(),
                    data: HashMap::new(),
                };

                let struct_iter = struct_type.fields.iter().zip(values.into_iter()).map(
                    |((name, type_id, field_index), val)| {
                        (name, field_index, type_id, val.into_ref())
                    },
                );

                for (name, key, type_id, val) in struct_iter {
                    match type_id {
                        Some(type_id)
                            if *type_id != val.get_type_id()
                                && val.get_type_id() != FenderTypeId::Null =>
                        {
                            return FenderValue::make_error(format!(
                                "Incorrect type used for field {}: expected `{}` found `{}`",
                                name,
                                type_id.to_string(),
                                val.get_type_id().to_string()
                            ))
                            .into();
                        }
                        _ => (),
                    }

                    ret.data.insert(*key as i64, InternalReference::from(val));
                }
                FenderValue::Struct(ret).into()
            }
            None => {
                FenderValue::make_error(format!("struct does not exist with internal id {}", id))
                    .into()
            }
        }
    }
}
