use crate::{
    fndr_native_func, type_match,
    type_sys::{
        fender_value::FenderValue::{self, *},
        type_id::FenderTypeId,
    },
};

fndr_native_func!(
    /// Get the `FenderValue::Type` from a string representing that type
    type_from_name_func,
    |_, value| {
        let value_str = type_match!(
            value {
                (String(v)) => v
            }
        );

        Ok(match FenderTypeId::type_from_str(&value_str.to_string()) {
            Some(t) => Type(t),
            None => {
                FenderValue::make_error(format!("No type found matching name `{}`", *value_str))
            }
        }
        .into())
    }
);

fndr_native_func!(
    /// Get the `type` of `value`
    get_type_func,
    |_, value| { Ok(Type(value.get_real_type_id()).into()) }
);

fndr_native_func!(
    /// Returns `Bool(true)` if `value` is `FenderValue::Reference`, otherwise return `Bool(false)`
    is_ref_func,
    |_, value| { Ok(Bool(matches!(value.get_type_id(), FenderTypeId::Reference)).into()) }
);
