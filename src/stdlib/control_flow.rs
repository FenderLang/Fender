use crate::{
    fender_value::FenderValue::{self, *},
    fndr_native_func,
};

fndr_native_func!(if_func, |ctx, cond, if_true, if_false| {
    let Bool(cond) = *cond else{
        return Ok(FenderValue::make_error(format!("`if` must be called with a condition value of type `Bool`\t`{:?}` was supplied", cond.get_type_id())).into());
    };

    if cond {
        Ok(match &*if_true {
            Function(if_true) => ctx.call(if_true, Vec::with_capacity(0))?,
            v => v.clone().into(),
        })
    } else {
        Ok(match &*if_false {
            Function(if_false) => ctx.call(if_false, Vec::with_capacity(0))?,
            v => v.clone().into(),
        })
    }
});

fndr_native_func!(else_func, |ctx, cond, body| {
    if !matches!(*cond, FenderValue::Null | FenderValue::Error(_)) {
        return Ok(cond);
    }

    Ok(match &*body {
        Function(f) => ctx.call(f, Vec::with_capacity(0))?,
        _ => body,
    })
});

fndr_native_func!(then_func, |ctx, cond, body| {
    if matches!(*cond, FenderValue::Null | FenderValue::Error(_)) {
        return Ok(cond);
    }

    if let FenderValue::Bool(false) = &*cond {
        return Ok(Null.into());
    }

    Ok(match &*body {
        Function(f) => {
            ctx.call(f, Vec::with_capacity(0))?;
            Bool(true)
        }
        b => Error(format!(
            "then body expected, found {:?}; else will be run if called",b.get_type_id()
        )),
    }
    .into())
});