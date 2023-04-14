#[derive(PartialEq, Debug, Clone, PartialOrd, Hash)]
pub enum FenderTypeId {
    Int,
    Float,
    Bool,
    String,
    Error,
    Null,
    Reference,
    Function,
    List,
    Char,
    Struct,
    Type,
    HashMap,
}

impl FenderTypeId {
    pub fn is_value_type(&self) -> bool {
        use FenderTypeId::*;
        match self {
            Int => true,
            Float => true,
            Bool => true,
            Error => false,
            Null => true,
            Reference => false,
            Function => true,
            String => false,
            List => false,
            Char => true,
            Struct => false,
            Type => true,
            HashMap => false,
        }
    }
    pub fn type_from_str(value: &str) -> Option<FenderTypeId> {
        match value {
            "Int" => Some(FenderTypeId::Int),
            "Float" => Some(FenderTypeId::Float),
            "Bool" => Some(FenderTypeId::Bool),
            "Error" => Some(FenderTypeId::Error),
            "Reference" => Some(FenderTypeId::Reference),
            "Function" => Some(FenderTypeId::Function),
            "String" => Some(FenderTypeId::String),
            "List" => Some(FenderTypeId::List),
            "Char" => Some(FenderTypeId::Char),
            "Struct" => Some(FenderTypeId::Struct),
            "Null" => Some(FenderTypeId::Null),
            "HashMap" => Some(FenderTypeId::HashMap),
            _ => None,
        }
    }
}

impl ToString for FenderTypeId {
    fn to_string(&self) -> String {
        use FenderTypeId::*;
        match self {
            Int => "Int",
            Float => "Float",
            Bool => "Bool",
            Error => "Error",
            Null => "Null",
            Reference => "Reference",
            Function => "Function",
            String => "String",
            List => "List",
            Char => "Char",
            Struct => "Struct",
            Type => "Type",
            HashMap => "HashMap",
        }
        .to_owned()
    }
}
