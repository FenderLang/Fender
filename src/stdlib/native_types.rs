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

/// Struct type loadable by `stdlib::load`
pub struct FenderNativeStructType {
    /// Name of struct type
    pub name: String,
    /// list of fields name and type constraint held by the struct
    pub fields: Vec<(String, Option<FenderTypeId>)>,
    /// constructor for the struct type
    pub constructor: FenderNativeFunction,
}

impl StdlibResource for FenderNativeStructType {
    fn load_into<const N: usize>(&self, engine: &mut ExecutionEngine<FenderTypeSystem>) -> usize {
        let fields = self
            .fields
            .iter()
            .cloned()
            .map(|(name, ty)| (engine.context.struct_table.field_index(&name), name, ty))
            .map(|(id, name, ty)| (name, ty, id))
            .collect::<Vec<(String, Option<FenderTypeId>, usize)>>();

        engine.context.struct_table.insert(FenderStructType {
            name: self.name.clone(),
            fields: fields,
        });

        self.constructor.load_into::<N>(engine)
    }
}
