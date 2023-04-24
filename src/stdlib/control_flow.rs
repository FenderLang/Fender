use crate::{
    fndr_native_func, type_match,
    type_sys::fender_value::FenderValue::{self, *},
};

fndr_native_func!(
    /// if `cond` is true will evaluate and return `if_true` else will evaluate and return `if_false
    if_func,
    |ctx, cond, if_true, if_false| {
        let cond = type_match!( cond {Bool(b) => b});

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
    }
);

fndr_native_func!(
    /// If called on `FenderValue::Null`, `FenderValue::Error`, or `FenderValue::Bool(false)` will evaluate and return `body`
    else_func,
    |ctx, cond, body| {
        if !matches!(*cond, FenderValue::Null | FenderValue::Error(_)) {
            return Ok(cond);
        }

        Ok(match &*body {
            Function(f) => ctx.call(f, Vec::with_capacity(0))?,
            _ => body,
        })
    }
);

fndr_native_func!(
    /// Conditional execution
    ///
    /// Executes `body` if `cond` is equivalent to `true`
    ///
    /// `body` must be of type `FenderValue::
    then_func,
    |ctx, cond, body| {
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
                "then body expected, found {:?}; else will be run",
                b.get_type_id()
            )),
        }
        .into())
    }
);

fndr_native_func!(
    /// Execute `body` while `cond` evaluates to true
    ///
    /// `cond` and `body` must be of type `FenderValue::Function`
    ///
    /// `cond` must return type `FenderValue::Bool`
    while_func,
    |ctx, cond, body| {
        let (cond, body) = type_match!(cond, body{
            (Function(cond), Function(body)) => (cond, body)
        });

        loop {
            let call_res = ctx.call(&cond, Vec::with_capacity(0))?;
            let keep_run = type_match!( call_res {Bool(b) => b});

            if !keep_run {
                return Ok(Bool(true).into());
            } else {
                ctx.call(&body, Vec::with_capacity(0))?;
            }
        }
    }
);

fndr_native_func!(
    /// Runs a function on a given value, then passes the original value
    also_func,
    |ctx, incoming_val, func| {
        type_match!(func {Function(f) => ctx.call(&f, vec![incoming_val.get_pass_object()])?});

        Ok(incoming_val)
    }
);

fndr_native_func!(
    /// Takes a value and a function and applies that function to the Runs a function on a given value, then passes the original value
    apply_func,
    |ctx, incoming_val, func| {
        let func = type_match!(func {Function(f) => f});

        ctx.call(&func, vec![incoming_val.get_pass_object()])
    }
);

fndr_native_func!(
    /// If `incoming_val` is of type `FenderValue::Error` run `func`
    ///
    /// otherwise return `incoming_val`
    ///
    /// if `func` is called `incoming_val` will be passed to it
    on_err_func,
    |ctx, incoming_val, func| {
        match incoming_val.unwrap_value() {
            Error(e) => (),
            _ => return Ok(incoming_val),
        };

        type_match!(
            func{
                Function(f) => {
                    let args = if f.arg_count().max().unwrap_or(1) >= 1{
                        vec![incoming_val]
                    }else{
                        Vec::new()
                    };

                    ctx.call(&f, args)
                }
            }
        )
    }
);

fndr_native_func!(
    /// If `incoming_val` is of type `FenderValue::Null` run `func`
    ///
    /// otherwise return `incoming_val`
    on_null_func,
    |ctx, incoming_val, func| {
        match incoming_val.unwrap_value() {
            Null => (),
            _ => return Ok(incoming_val),
        };

        type_match!(
            func {
                Function(f) => ctx.call(&f, Vec::new())
            }
        )
    }
);

fndr_native_func!(
    /// If `incoming_val` is of type `type_id` run `func`
    ///
    /// otherwise return `incoming_val`
    ///
    /// if `func` is called `incoming_val` will be passed to it
    on_type_func,
    |ctx, incoming_val, type_id, func| {
        type_match!(
            type_id, func{
                (Type(ty), Function(f)) => {
                    if incoming_val.get_real_type_id() == ty {
                        let args = if f.arg_count().max().unwrap_or(1) >= 1{
                            vec![incoming_val]
                        }else{
                            Vec::new()
                        };
                        ctx.call(&f, args)
                    }else{
                        Ok(incoming_val)
                    }
                }
            }
        )
    }
);
