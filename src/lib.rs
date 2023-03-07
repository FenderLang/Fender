#![allow(dead_code)]

use freight_vm::TypeSystem;

// move these to a prelude or just get rid of them
pub use fender_reference::FenderReference;
pub use fender_value::FenderValue;
pub use type_sys::{type_id::FenderTypeId, type_system::FenderTypeSystem};

pub mod fender_reference;
pub mod fender_value;
pub mod type_sys {
    pub mod type_id;
    pub mod type_system;
}
pub mod interpreter;
pub mod lazy_cell;
pub mod operators;
pub mod stdlib;
#[cfg(test)]
mod test;

#[macro_export]
macro_rules! count {
    ($first:pat_param, $($rest:pat_param),*) => {
        1 + $crate::count!($($rest),*)
    };
    ($first:pat_param) => {1};
    () => {0};
}

#[macro_export]
macro_rules! fndr_native_func {
    ($name:ident, | $ctx:tt $(, $($arg:pat_param),*)? | $body:expr) => {
        #[allow(unused_variables)]
        pub fn $name($ctx: &mut freight_vm::execution_engine::ExecutionEngine<$crate::type_sys::type_system::FenderTypeSystem>, args: Vec<$crate::fender_reference::FenderReference>) -> Result<$crate::fender_reference::FenderReference, freight_vm::error::FreightError> {
            const _ARG_COUNT: usize = $crate::count!($($($arg),*)?);
            $(
                    let [$($arg),*]: [$crate::fender_reference::FenderReference; _ARG_COUNT]  = args.try_into().unwrap();
                    )?
            $body
        }
    }
}
