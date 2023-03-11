#[derive(PartialEq, Debug)]
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
        }
        .to_owned()
    }
}
