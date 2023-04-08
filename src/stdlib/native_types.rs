use crate::{
    fender_reference::FenderReference,
    fender_value::{fender_structs::FenderStructType, FenderValue},
    stdlib::loader::StdlibResource,
    type_sys::{type_id::FenderTypeId, type_system::FenderTypeSystem},
};
use freight_vm::{
    error::FreightError,
    execution_engine::ExecutionEngine,
    expression::{Expression, NativeFunction},
    function::{ArgCount, FunctionRef},
};
