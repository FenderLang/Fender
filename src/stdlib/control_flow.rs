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
            "then body expected, found {:?}; else will be run if called",
            b.get_type_id()
        )),
    }
    .into())
});

fndr_native_func!(while_func, |ctx, cond, body| {
    let (Function(cond), Function(body)) = (&*cond, &*body) else
    {
        return Ok(FenderValue::make_error(format!("while must take 2 expressions `{:?}` and `{:?}` were provided", cond.get_type_id(), body.get_type_id())).into());
    };
    loop {
        let keep_run = match &*ctx.call(cond, Vec::with_capacity(0))? {
            Bool(bool_val) => *bool_val,
            v => {
                return Ok(FenderValue::make_error(format!(
                    "condition expression did not evaluate to a boolean value, ",
                ))
                .into());
            }
        };

        if !keep_run {
            return Ok(Null.into());
        } else {
            ctx.call(body, Vec::with_capacity(0))?;
        }
    }
});
