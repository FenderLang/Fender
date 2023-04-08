use super::functions::FenderNativeFunction;
use crate::{
    fender_value::fender_structs::FenderStructType,
    stdlib::loader::StdlibResource,
    type_sys::{type_id::FenderTypeId, type_system::FenderTypeSystem},
};
use freight_vm::execution_engine::ExecutionEngine;

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

#[macro_export]
/// converts ident to Optional `FenderTypeId`
macro_rules! option_fender_type_id {
    (None) => {
        None
    };

    ($name:ident) => {
        Some($crate::type_sys::type_id::FenderTypeId::$name)
    };
}

#[macro_export]
/// Create fender function in rust
macro_rules! fndr_native_struct {
    (
        $(#[$docs:meta])*
        $name:ident, {$($field_name:ident:$field_type:ident),*}, $constructor:expr
        // $name:ident, | $ctx:tt $(, $($arg:pat_param),*)? | $body:expr
    ) => {
        $(#[$docs])*
        #[allow(unused)]
        $crate::stdlib::native_types::structs::FenderNativeStructType{
            name: stringify!($name).to_owned(),
            fields: vec![
                $(
                    (stringify!($field_name).to_owned(), $crate::option_fender_type_id!($field_type)),
                )*
            ],
            constructor: FenderNativeFunction{func:$constructor, args: fixed($crate::count!($($field_name),*)) }
        }
    }
}
#[macro_export]
/// Create default constructor for native structs
macro_rules! create_default_struct_constructor {
    ($constructor_name:ident, $name:ident, {$($field_name:ident),*}) => {
        fndr_native_func!(
            #[allow(missing_docs)]
            $constructor_name,
            |ctx, $($field_name),*| {
                Ok($crate::fender_value::FenderValue::Struct($crate::fender_value::fender_structs::FenderStruct {
                    struct_id: {
                        let __id = *ctx
                            .context
                            .struct_table
                            .struct_name_index()
                            .get(stringify!($name))
                            .unwrap();
                        ctx.context.struct_table.type_list()[__id].clone()
                    },
                    data: {
                        let mut __data = std::collections::HashMap::new();
                        $(
                            __data.insert(ctx.context.struct_table.field_index(stringify!($field_name)) as i64, $field_name.into());
                        )*
                        __data
                    },
                })
                .into())
            }
        );
    };
}
