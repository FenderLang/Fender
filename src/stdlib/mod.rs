use crate::{fndr_native_func, FenderReference, FenderValue};

pub mod loader;

fndr_native_func!(if_func, |ctx, cond, if_true, if_false| {
    use FenderValue::*;
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
    Ok(Default::default())
});

fndr_native_func!(println_func, |_, item| {
    println!("{}", item.to_string());
    Ok(Default::default())
});

fndr_native_func!(read_line_func, |ctx| {
    use FenderValue::*;

    let mut buf = std::string::String::new();
    match std::io::stdin().read_line(&mut buf) {
        Ok(_) => Ok(String(buf).into()),
        Err(e) => Ok(Error(e.to_string()).into()),
    }
});

fndr_native_func!(get_raw_func, |ctx, item| Ok(item.unwrap_value().into()));

fndr_native_func!(len_func, |ctx, item| {
    use FenderValue::*;
    Ok(match item.len() {
        Ok(len) => Int(len as i64),
        Err(e_str) => Error(e_str),
    }
    .into())
});
