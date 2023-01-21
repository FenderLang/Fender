use std::{rc::Rc, cell::UnsafeCell};

type Stack = Vec<Value>;

struct Scope {
    values: usize,
    statements: Vec<Statement>,
}

#[derive(Clone, Debug)]
enum Value {
    Reference(Rc<UnsafeCell<Value>>),
    Integer(i64),
    Float(f64),
    String(String),
    Null,
    // TODO: Rest of values
}

enum Statement {
    Expression(Expression),
}

enum Expression {
    Constant(Value),
    Variable(usize),
    Function(fn(&mut Stack) -> Value),
    // TODO: Rest of expressions
}

impl Expression {
    fn evaluate(&self, stack: &mut Stack) -> Value {
        use Expression::*;
        match self {
            Constant(v) => v.clone(),
            Variable(index) => stack[stack.len() - index].clone(),
            Function(func) => func(stack),
        }
    }
}