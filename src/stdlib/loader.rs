use crate::type_sys::type_system::FenderTypeSystem;
use freight_vm::{execution_engine::ExecutionEngine, function::FunctionWriter};

#[macro_export]
/// Resolves the name of a dependency as a string
macro_rules! dep_name {
    ($name:literal => $variant_name:ident) => {
        $name
    };
    ($variant_name:ident) => {
        stringify!($variant_name)
    };
}

#[macro_export]
/// Generates implementations for standard library resources that can be loaded
macro_rules! deps_enum {
    ($name:ident, $count:ident : $($(@ $string_name:literal)? $val:ident $([$($dep:ident),*])? => $res:expr),* $(,)?) => {
        /// Number of valid dependencies
        pub const $count: usize = $crate::count!($($val),*);
        #[allow(non_camel_case_types)]
        #[derive(Clone, Copy)]
        /// Standard library resources that can be loaded
        pub enum $name {
            $(#[allow(missing_docs)] $val),*
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
                engine: &mut ExecutionEngine<FenderTypeSystem>,
            ) -> usize {
                if let Some(dep) = engine.context.deps.0[*self as usize] {
                    return dep;
                }
                match self {
                    $(
                        $name::$val => {
                            $($(
                                $name::$dep.load(engine);
                            )*)?
                            let loaded = $res.load_into::<N>(engine);
                            engine.context.deps.0[*self as usize] = Some(loaded);
                            loaded
                        }
                    ),*
                }
            }
        }
        impl From<$name> for usize{
            fn from(val:$name) -> Self{
                val as usize
            }
        }
    };
}

#[derive(Debug)]
/// Represents a list of loaded standard library modules pointing to their location
/// in global variables
pub struct DependencyList<const N: usize>(pub(crate) [Option<usize>; N]);

impl<const N: usize> Default for DependencyList<N> {
    fn default() -> Self {
        Self([None; N])
    }
}

/// Represents a standard library resource that can be loaded
pub trait StdlibResource {
    /// Load this resource into the VM and return the address of the global variable it resides in
    fn load_into<const N: usize>(&self, engine: &mut ExecutionEngine<FenderTypeSystem>) -> usize;
}
