use std::collections::HashMap;

use crate::{
    fender_value::{fender_structs::FenderStruct, FenderValue},
    fndr_native_func,
};

fndr_native_func!(
    ///
    test_constructor,
    |ctx, name, age| {
        let name_index = *ctx
            .context
            .struct_table
            .struct_name_index()
            .get("StdLibTest")
            .unwrap();
        let type_id = ctx.context.struct_table.type_list()[name_index].clone();

        let name_field_id = ctx.context.struct_table.field_index("name");
        let age_field_id = ctx.context.struct_table.field_index("age");

        let mut data = HashMap::new();

        data.insert(name_field_id as i64, name.into());
        data.insert(age_field_id as i64, age.into());

        Ok(FenderValue::Struct(FenderStruct {
            struct_id: type_id,
            data,
        })
        .into())
    }
);
