
pub enum TypeId {
    Int,
    Float,
    Bool,
    Error,
    Null,
    Reference,
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
