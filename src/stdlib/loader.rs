#![allow(missing_docs)]
use freight_vm::{function::FunctionWriter, vm_writer::VMWriter};

use crate::type_sys::type_system::FenderTypeSystem;

#[macro_export]
macro_rules! dep_name {
    ($name:literal => $variant_name:ident) => {
        $name
    };
    ($variant_name:ident) => {
        stringify!($variant_name)
    };
}

#[macro_export]
macro_rules! deps_enum {
    ($name:ident, $count:ident : $($(@ $string_name:literal)? $val:ident => $res:expr),* $(,)?) => {
        /// Number of valid dependencies
        pub const $count: usize = $crate::count!($($val),*);
        #[allow(non_camel_case_types, missing_docs)]
        pub enum $name {
            $($val),*
        }
        impl $name {
            fn from_str(s: &str) -> Option<$name> {
                match s {
                    $(dep_name!($($string_name =>)? $val) => Some($name::$val)),*,
                    _ => None
                }
            }

            fn all() -> impl IntoIterator<Item = $name> {
                [$($name::$val),*]
            }

            fn load(&self, vm: &mut freight_vm::vm_writer::VMWriter<FenderTypeSystem>, func: &mut freight_vm::function::FunctionWriter<FenderTypeSystem>) -> usize {
                match self {
                    $(
                        $name::$val => $res.load_into(vm, func)
                    ),*
                }
            }
        }
        impl Into<usize> for $name {
            fn into(self) -> usize {
                self as usize
            }
        }
    };
}

pub struct DependencyList<const N: usize>([bool; N]);

impl<const N: usize> DependencyList<N> {
    pub fn add_deps(&mut self, deps: impl IntoIterator<Item = impl Into<usize>>) {
        for dep in deps {
            self.0[dep.into()] = true;
        }
    }
}

pub trait StdlibResource {
    fn load_into(
        &self,
        writer: &mut VMWriter<FenderTypeSystem>,
        main: &mut FunctionWriter<FenderTypeSystem>,
    ) -> usize;
    fn set_deps<const N: usize>(&self, deps: &mut DependencyList<N>);
}
