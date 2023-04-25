use freight_vm::TypeSystem;

pub mod type_sys {
    pub mod fender_reference;
    pub mod fender_value;
    pub mod freight_type_system;
    pub mod type_id;
}
pub mod error;
pub mod interpreter;
pub mod lazy_cell;
pub mod operators;
pub mod plugin;
/// The freight standard library
pub mod stdlib;

/// re-export
pub use type_sys::fender_value;

#[cfg(test)]
mod test;
