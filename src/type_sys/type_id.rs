pub enum TypeId {
    Int,
    Float,
    Bool,
    Error,
    Null,
    Reference,
}

impl TypeId {
    pub fn is_primitive(&self) -> bool {
        match self {
            TypeId::Int => true,
            TypeId::Float => true,
            TypeId::Bool => true,
            TypeId::Error => false,
            TypeId::Null => true,
            TypeId::Reference => false,
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
