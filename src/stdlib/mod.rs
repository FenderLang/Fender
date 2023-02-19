use crate::{fndr_native_func, FenderValue, FenderReference};

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
        ctx.call_function(if_true.clone(), vec![])?
    } else {
        ctx.call_function(if_false.clone(), vec![])?
    })
});

fndr_native_func!(print_func, |_, item| {
    println!("{:?}", *item);
    Ok(Default::default())
});