use std::io::Write;

use crate::{fndr_native_func, FenderReference};

pub mod loader;

fndr_native_func!(if_func, |ctx, cond, if_true, if_false| {
    let (Bool(cond), Function(if_true), Function(if_false)) = (&*cond, &*if_true, &*if_false) else {
        return Ok(FenderReference::FRaw(Error(format!(
                "Invalid argument types {:?} {:?} {:?}",
            cond.get_type_id(),
            if_true.get_type_id(),
            if_false.get_type_id()
        ))));
    };
    Ok(if *cond {
        ctx.call(if_true, Vec::with_capacity(0))?
    } else {
        ctx.call(if_false, Vec::with_capacity(0))?
    })
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
            _ => Error(format!("Invalid int string: {}", s)).into()
        },
        _ => Error(format!("Cannot convert {} to int", item.get_real_type_id().to_string())).into(),
    })
});