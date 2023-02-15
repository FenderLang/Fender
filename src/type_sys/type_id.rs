pub enum FenderTypeId {
    Int,
    Float,
    Bool,
    Error,
    Null,
    Reference,
    Function,
}

impl FenderTypeId {
    pub fn is_primitive(&self) -> bool {
        match self {
            FenderTypeId::Int => true,
            FenderTypeId::Float => true,
            FenderTypeId::Bool => true,
            FenderTypeId::Error => false,
            FenderTypeId::Null => true,
            FenderTypeId::Reference => false,
            FenderTypeId::Function => true,
        }
    }
}

impl ToString for FenderTypeId {
    fn to_string(&self) -> String {
        match self {
            FenderTypeId::Int => "Int",
            FenderTypeId::Float => "Float",
            FenderTypeId::Bool => "Bool",
            FenderTypeId::Error => "Error",
            FenderTypeId::Null => "Null",
            FenderTypeId::Reference => "Reference",
            FenderTypeId::Function => "Function",
        }
        .to_owned()
    }
}
