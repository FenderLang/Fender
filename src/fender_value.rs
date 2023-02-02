use crate::fender_reference::InternalReference;

#[derive(Clone, Debug, Default)]
pub enum FenderValue {
    Ref(InternalReference),
    Int(i64),
    Float(f64),
    Bool(bool),
    Error(String),
    #[default]
    Null,
}
