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
    ($name:ident, $count:ident : $($(@ $string_name:literal)? $val:ident $([$($dep:ident),*])? => $res:expr),* $(,)?) => {
        /// Number of valid dependencies
        pub const $count: usize = $crate::count!($($val),*);
        #[allow(non_camel_case_types, missing_docs)]
        #[derive(Clone, Copy)]
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

            /// Get the name of this standard library resource
            pub fn name(&self) -> &'static str {
                match self {
                    $($name::$val => dep_name!($($string_name =>)? $val)),*
                }
            }

            /// Get all entries in the standard library
            pub fn all() -> impl IntoIterator<Item = $name> {
                [$($name::$val),*]
            }

            fn load<const N: usize>(
                &self,
                vm: &mut freight_vm::vm_writer::VMWriter<FenderTypeSystem>,
                func: &mut freight_vm::function::FunctionWriter<FenderTypeSystem>,
                deps: &mut DependencyList<N>,
            ) -> usize {
                if let Some(dep) = deps.0[*self as usize] {
                    return dep;
                }
                match self {
                    $(
                        $name::$val => {
                            $($(
                                $name::$dep.load(vm, func, deps);
                            )*)?
                            let loaded = $res.load_into(vm, func, deps);
                            deps.0[*self as usize] = Some(loaded);
                            loaded
                        }
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

#[derive(Debug)]
pub struct DependencyList<const N: usize>(pub(crate) [Option<usize>; N]);

impl<const N: usize> Default for DependencyList<N> {
    fn default() -> Self {
        Self([None; N])
    }
}

pub trait StdlibResource {
    fn load_into<const N: usize>(
        &self,
        writer: &mut VMWriter<FenderTypeSystem>,
        main: &mut FunctionWriter<FenderTypeSystem>,
        deps: &mut DependencyList<N>,
    ) -> usize;
}
