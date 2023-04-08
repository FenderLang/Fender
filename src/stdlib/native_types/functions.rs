use crate::{
    fender_reference::FenderReference, fender_value::FenderValue, stdlib::loader::StdlibResource,
    type_sys::type_system::FenderTypeSystem,
};
use freight_vm::{
    error::FreightError,
    execution_engine::ExecutionEngine,
    expression::{Expression, NativeFunction},
    function::{ArgCount, FunctionRef},
};

/// Function loadable by `stdlib::load`
pub struct FenderNativeFunction {
    /// function to be run
    pub func: fn(
        &mut ExecutionEngine<FenderTypeSystem>,
        Vec<FenderReference>,
    ) -> Result<FenderReference, FreightError>,
    /// number of args
    pub args: ArgCount,
}

impl StdlibResource for FenderNativeFunction {
    fn load_into<const N: usize>(&self, engine: &mut ExecutionEngine<FenderTypeSystem>) -> usize {
        let global = engine.create_global();
        let func = FunctionRef::new_native(global, NativeFunction::new(self.func), self.args);
        engine
            .evaluate(
                &Expression::AssignGlobal(global, Box::new(FenderValue::Function(func).into())),
                &mut [],
                &[],
            )
            .expect("Assigning value should never fail");
        global
    }
}

#[macro_export]
/// Create fender function in rust
macro_rules! fndr_native_func {
    (
        $(#[$docs:meta])*
        $name:ident, | $ctx:tt $(, $($arg:pat_param),*)? | $body:expr
    ) => {
        $(#[$docs])*
        #[allow(unused)]
        pub fn $name(
            $ctx: &mut freight_vm::execution_engine::ExecutionEngine<
                $crate::type_sys::type_system::FenderTypeSystem
            >,
            args: Vec<$crate::fender_reference::FenderReference>,
        ) -> Result<$crate::fender_reference::FenderReference, freight_vm::error::FreightError> {
            const _ARG_COUNT: usize = $crate::count!($($($arg),*)?);
            $(
                    let [$($arg),*]: [$crate::fender_reference::FenderReference; _ARG_COUNT]  = args.try_into().unwrap();
                    )?
            $body
        }
    }
}
