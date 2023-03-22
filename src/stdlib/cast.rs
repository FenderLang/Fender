use crate::{
    fender_value::FenderValue::{self, *},
    fndr_native_func,
};

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
                _ => FenderValue::make_error(format!("Invalid int string: {}", s)).into(),
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
    |_, item| { Ok(String(item.to_string()).into()) }
);

fndr_native_func!(
    /// Cast `FenderValue` to `FenderValue::String`
    to_bool_func,
    |_, item| {
        Ok(item.to_bool().into())
    }
);
