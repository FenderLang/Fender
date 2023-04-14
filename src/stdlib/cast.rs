use crate::{
    fender_reference::InternalReference,
    fender_value::FenderValue::{self, *},
    fndr_native_func,
};
use std::ops::Deref;

fndr_native_func!(
    /// return a raw `FenderValue` unwrapping all references
    get_raw_func,
    |_, item| Ok(item.unwrap_value().into())
);

fndr_native_func!(
    /// Cast `FenderValue` to `FenderValue::Int`
    int_func,
    |_, item| {
        Ok(match &*item {
            String(s) => match s.parse() {
                Ok(i) => Int(i).into(),
                _ => FenderValue::make_error(format!("Invalid int string: {}", s.deref())).into(),
            },
            Int(val) => Int(*val).into(),
            Float(val) => Int(*val as i64).into(),
            Bool(val) => Int(*val as i64).into(),
            Char(val) => Int(*val as i64).into(),
            _ => FenderValue::make_error(format!(
                "Cannot convert {} to int",
                item.get_real_type_id().to_string()
            ))
            .into(),
        })
    }
);

fndr_native_func!(
    /// Cast `FenderValue` to `FenderValue::String`
    str_func,
    |_, item| { Ok(String(item.to_string().into()).into()) }
);

fndr_native_func!(
    /// Cast `FenderValue` to `FenderValue::String`
    to_bool_func,
    |_, item| { Ok(item.to_bool().into()) }
);

fndr_native_func!(
    /// Wrap `value` in a `Ref`
    to_ref_func,
    |_, value| { Ok(Ref(InternalReference::new(value)).into()) }
);

fndr_native_func!(
    /// Cast to a list
    to_list_func,
    |ctx, value| {
        Ok(value
            .cast_to(crate::type_sys::type_id::FenderTypeId::List, ctx)?
            .into())
    }
);

fndr_native_func!(
    /// Cast to a list
    join_to_string_func,
    |ctx, value| { Ok(value.join_to_string().into()) }
);
