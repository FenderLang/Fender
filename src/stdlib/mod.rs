use crate::fender_value::FenderValue::{self, *};
use crate::fndr_native_func;
use std::io::Write;

pub mod loader;

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

fndr_native_func!(print_func, |_, item| {
    print!("{}", item.to_string());
    let mut lock = std::io::stdout().lock();
    let _ = lock.flush();
    Ok(Default::default())
});

fndr_native_func!(println_func, |_, item| {
    println!("{}", item.to_string());
    Ok(Default::default())
});

fndr_native_func!(read_line_func, |ctx| {
    match std::io::stdin().lines().next() {
        Some(Ok(s)) => Ok(String(s).into()),
        Some(Err(e)) => Ok(Error(e.to_string()).into()),
        None => Ok(Error("End of file".to_string()).into()),
    }
});

fndr_native_func!(get_raw_func, |ctx, item| Ok(item.unwrap_value().into()));

fndr_native_func!(len_func, |ctx, item| {
    Ok(match item.len() {
        Ok(len) => Int(len as i64),
        Err(e_str) => Error(e_str),
    }
    .into())
});

fndr_native_func!(int_func, |ctx, item| {
    Ok(match &*item {
        String(s) => match s.parse() {
            Ok(i) => Int(i).into(),
            _ => Error(format!("Invalid int string: {}", s)).into(),
        },
        Int(val) => Int(*val).into(),
        Float(val) => Int(*val as i64).into(),
        Bool(val) => Int(*val as i64).into(),
        Char(val) => Int(*val as i64).into(),
        _ => Error(format!(
            "Cannot convert {} to int",
            item.get_real_type_id().to_string()
        ))
        .into(),
    })
});

fndr_native_func!(read_func, |ctx, file_name| {
    let String(file_name) = &*file_name else {
        return Ok(Error("file name must be of type `String`".into()).into());
    };
    Ok(match std::fs::read_to_string(file_name) {
        Ok(s) => String(s).into(),
        Err(e) => FenderValue::make_error(format!("failed to read file due to error: {e}")).into(),
    })
});

fndr_native_func!(write_func, |ctx, data, file_name| {
    let String(file_name) = &*file_name else {
        return Ok(Error("file name must be of type `String`".into()).into());
    };
    Ok(match std::fs::write(file_name, data.to_string()) {
        Ok(s) => Null.into(),
        Err(e) => FenderValue::make_error(format!("failed to read file due to error: {e}")).into(),
    })
});
