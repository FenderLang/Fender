use std::ops::Deref;

use freight_vm::{function::FunctionRef, value::Value};

use crate::{
    create_default_struct_constructor,
    fender_reference::{FenderReference, InternalReference},
    fender_value::FenderValue,
    fndr_native_func,
    stdlib::{self, STDLIB_SIZE},
    type_sys::type_id::FenderTypeId,
};

/// Iterator over list
pub mod list;

fndr_native_func!(
    /// create iterator from given `value`
    iter_func,
    |ctx, value| {
        match value.unwrap_value() {
            FenderValue::List(_) => {
                let constructor_id = stdlib::load::<STDLIB_SIZE>("__list_next", ctx).unwrap();
                list::list_iter_constructor(
                    ctx,
                    vec![
                        FenderValue::Function(
                            ctx.get_global(constructor_id)
                                .cast_to_function()
                                .unwrap()
                                .clone(),
                        )
                        .into(),
                        value.dupe_ref(),
                        FenderReference::FRef(InternalReference::new(FenderValue::Int(0))),
                    ],
                )
            }
            FenderValue::Int(_) => todo!(),
            FenderValue::Float(_) => todo!(),
            FenderValue::Char(_) => todo!(),
            FenderValue::String(_) => todo!(),
            FenderValue::Bool(_) => todo!(),
            FenderValue::Error(_) => todo!(),
            FenderValue::Function(_) => todo!(),
            FenderValue::Struct(_) => todo!(),
            FenderValue::Type(_) => todo!(),
            FenderValue::HashMap(_) => todo!(),
            FenderValue::Null => todo!(),
            FenderValue::Ref(_) => unreachable!(),
        }
    }
);

fndr_native_func!(
    /// call next on iterator
    next_func,
    |ctx, iter_struct| {
        let iter = match iter_struct.unwrap_value() {
            FenderValue::Struct(s)
                if s.struct_id
                    .fields
                    .iter()
                    .find(|(f_name, typ, _)| {
                        f_name == "next_func"
                            && typ.is_some()
                            && typ.clone().unwrap() == FenderTypeId::Function
                    })
                    .is_some() =>
            {
                s
            }
            e => {
                return Ok(FenderValue::make_error(format!(
                    "`next` must be run on an iterator: Expected iterator instead found `{}`",
                    e.fender_dbg_string()
                ))
                .into())
            }
        };

        let field_id = ctx.context.struct_table.field_index("next_func");

        ctx.call(
            iter.data[&(field_id as i64)]
                .deref()
                .cast_to_function()
                .unwrap(),
            vec![iter_struct],
        )
    }
);

create_default_struct_constructor!(
    iter_result_constructor, IterRes, {valid, data}
);
