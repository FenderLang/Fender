use std::{cell::RefCell, rc::Rc};

use freight_vm::value::Value;

use crate::{
    fender_reference::FenderReference, fender_value::FenderValue, fndr_native_func,
    iterator::FenderIterator,
};

macro_rules! expect_iterable {
    ($var_name:ident => $iter_name:ident) => {
        let Some($iter_name) = $var_name.iter() else {
                    return Ok(FenderValue::make_error(
                        format!("Type `{}` is not iterable", $var_name.get_type_id().to_string()))
                        .into());
                };
    };
}

macro_rules! expect_func {
    ($var_name:ident => $func_name:ident) => {
        let Some($func_name) = $var_name.cast_to_function() else {
            return Ok(FenderValue::make_error(
                format!("Expected `Function` as second argument, got `{}`", $var_name.get_type_id().to_string()))
                .into());
        };
    }
}

fndr_native_func!(
    /// Transform elements in an iterable type using a mapping function
    map_func,
    |_, list, func| {
        expect_iterable!(list => iter);
        expect_func!(func => func);
        let func = func.clone();
        let iter = Rc::new(RefCell::new(iter));
        let iter2 = iter.clone();
        Ok(FenderIterator {
            next: Rc::new(move |e| {
                let elem = (iter.borrow().next)(e)?;
                e.call(&func, vec![elem])
            }),
            has_next: Rc::new(move |e| (iter2.borrow().has_next)(e)),
        }
        .into())
    }
);

fndr_native_func!(
    /// Remove elements according to a filtering function
    filter_func,
    |_, list, pred| {
        expect_iterable!(list => iter);
        expect_func!(pred => pred);
        let next: Rc<RefCell<Option<FenderReference>>> = Rc::new(RefCell::new(None));
        let next2 = next.clone();
        let iter2 = iter.clone();
        let pred2 = pred.clone();
        Ok((FenderIterator {
            next: Rc::new(move |e| {
                if let Some(val) = std::mem::take(&mut *next.borrow_mut()) {
                    Ok(val.clone())
                } else {
                    Ok(Default::default())
                }
            }),
            has_next: Rc::new(move |e| {
                if next2.borrow().is_some() {
                    Ok(FenderValue::Bool(true).into())
                } else {
                    while *(iter2.has_next)(e)? == FenderValue::Bool(true) {
                        let elem = (iter2.next)(e)?;
                        if e.call(&pred2, vec![elem.clone()])?.to_bool() == FenderValue::Bool(true)
                        {
                            next2.replace(Some(elem));
                            break;
                        }
                    }
                    Ok(FenderValue::Bool(next2.borrow().is_some()).into())
                }
            }),
        }
        .into()))
    }
);

fndr_native_func!(
    /// Reduce an iterator down to a single value with a starting value and accumulator
    reduce_func,
    |e, list, mut init, accum| {
        expect_iterable!(list => iter);
        expect_func!(accum => accum);
        while *(iter.has_next)(e)? == FenderValue::Bool(true) {
            let elem = (iter.next)(e)?;
            init = e.call(&accum, vec![init, elem])?;
        }
        Ok(init)
    }
);

fndr_native_func!(
    /// Run a function on each element of an iterator
    each_func,
    |e, list, func| {
        expect_iterable!(list => iter);
        expect_func!(func => func);
        while *(iter.has_next)(e)? == FenderValue::Bool(true) {
            let elem = (iter.next)(e)?;
            e.call(&func, vec![elem])?;
        }
        Ok(Default::default())
    }
);
