use std::sync::Arc;
use std::cell::RefCell;
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub struct Identifier(usize);

#[derive(Clone, Debug)]
pub struct Reference(Arc<RefCell<Value>>);

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

#[derive(Clone, Debug)]
pub enum Statement {
    Expr(Expression),
    Assignment(Expression, Expression),
    Declaration(Reference, Expression),
}

#[derive(Clone, Debug)]
pub struct Expression {

}

pub trait LazyValue {
    fn evaluate(&self, scope: Scope) -> Reference;
}

#[derive(Clone, Debug)]
pub struct Scope {
    statements: Vec<Statement>,
    vars: Vec<Reference>,
}