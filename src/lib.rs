use std::sync::Arc;
use std::cell::RefCell;
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub struct Identifier(usize);

#[derive(Clone, Debug)]
pub struct Reference(Arc<RefCell<Value>>);

impl Reference {
    fn new(value: Value) -> Reference {
        Reference(Arc::new(RefCell::new(value)))
    }
}

#[derive(Clone, Debug)]
pub struct Primitive {
    name: String,
    id: usize,
}

#[derive(Clone, Debug)]
pub struct StructType {
    name: String,
    id: usize,
    fields: HashMap<Identifier, usize>,
    types: Vec<Type>,
}

#[derive(Clone, Debug)]
pub enum Type {
    Primitive(Primitive),
    Struct(StructType),
}

#[derive(Clone, Debug)]
pub enum Value {
    Int(i64),
    Float(f64),
    String(String),
    Boolean(bool),
    List(Vec<Value>),
    Function(Scope),
    Struct(Identifier, Vec<Reference>)
}

impl Default for Value {
    fn default() -> Self {
        Value::Int(0)
    }
}

#[derive(Clone, Debug)]
pub enum Statement {
    Expr(Expression),
    Assignment(Expression, Expression),
    Declaration(Reference, Expression),
}

#[derive(Clone, Debug)]
pub struct Expression {

}

pub struct Operator<const N: usize> {
    pub operate: fn([Value; N]) -> Value,
    pub symbol: &'static str,
}

pub enum Operation {
    BinaryOperation(Operator<2>, Box<dyn LazyValue>, Box<dyn LazyValue>),
    UnaryOperation(Operator<1>, Box<dyn LazyValue>)
}

impl LazyValue for Operation {
    fn evaluate_value(&self, scope: &Scope) -> Value {
        match self {
            Self::BinaryOperation(op, left, right) => {
                (op.operate)([left.evaluate_value(scope), right.evaluate_value(scope)])
            },
            Self::UnaryOperation(op, operand) => {
                (op.operate)([operand.evaluate_value(scope)])
            },
        }
    }
}

pub trait LazyValue {
    fn evaluate_value(&self, scope: &Scope) -> Value;

    fn evaluate(&self, scope: &Scope) -> Reference {
        Reference::new(self.evaluate_value(scope))
    }

    fn is_const(&self) -> bool {
        false
    }
}

#[derive(Clone, Debug)]
pub struct Scope {
    statements: Vec<Statement>,
    vars: Vec<Reference>,
}