#![allow(missing_docs)]
use freight_vm::{function::FunctionWriter, vm_writer::VMWriter};

use crate::type_sys::type_system::FenderTypeSystem;

#[macro_export]
macro_rules! deps_enum {
    ($name:ident, $count:ident : $($val:ident),*) => {
        /// Number of valid dependencies
        pub const $count: usize = $crate::count!($($val),*);
        #[allow(non_camel_case_types, missing_docs)]
        pub enum $name {
            $($val),*
        }
        impl $name {
            fn from_str(s: &str) -> Option<$name> {
                match s {
                    $(stringify!($val) => Some($name::$val)),*,
                    _ => None
                }
            }

            fn all() -> impl IntoIterator<Item = $name> {
                [$($name::$val),*]
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
