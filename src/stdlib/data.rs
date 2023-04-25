use crate::{
    fndr_native_func,
    operators::FenderBinaryOperator,
    type_match,
    type_sys::{
        fender_reference::FenderReference,
        fender_value::{iterator::FenderIterator, FenderValue},
    },
};
use freight_vm::{operators::BinaryOperator, value::Value};
use std::{cell::RefCell, ops::Deref, rc::Rc};

macro_rules! expect_iterable {
    ($var_name:ident => $iter_name:ident) => {
        let Some($iter_name) = $var_name.try_iter() else {
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
        Ok(FenderIterator {
            next: Rc::new(move |e| {
                let elem = (iter.next)(e)?;
                e.call(&func, vec![elem])
            }),
            has_next: iter.has_next.clone(),
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
        let iter2 = iter;
        let pred2 = pred.clone();
        Ok(FenderIterator {
            next: Rc::new(move |e| {
                if let Some(val) = std::mem::take(&mut *next.borrow_mut()) {
                    Ok(val)
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
        .into())
    }
);

fndr_native_func!(
    /// Reduce an iterator down to a single value with a starting value and accumulator
    reduce_func,
    |ctx, list, mut init, accum| {
        expect_iterable!(list => iter);
        expect_func!(accum => accum);
        while *(iter.has_next)(ctx)? == FenderValue::Bool(true) {
            let elem = (iter.next)(ctx)?;
            init = ctx.call(accum, vec![init, elem])?;
        }
        Ok(init)
    }
);

fndr_native_func!(
    /// Reduce an iterator, but pass each intermediate reduction step forward
    scan_func,
    |ctx, list, mut init, accum| {
        expect_iterable!(list => iter);
        expect_func!(accum => accum);
        let accum = accum.clone();
        let init = RefCell::new(init);
        Ok(FenderIterator {
            next: Rc::new(move |e| {
                let elem = (iter.next)(e)?;
                let mut borrow = init.borrow_mut();
                *borrow = e.call(&accum, vec![borrow.clone(), elem])?;
                Ok(borrow.clone())
            }),
            has_next: iter.has_next.clone(),
        }
        .into())
    }
);

fndr_native_func!(
    /// Run a function on each element of an iterator
    each_func,
    |ctx, list, func| {
        expect_iterable!(list => iter);
        expect_func!(func => func);
        while *(iter.has_next)(ctx)? == FenderValue::Bool(true) {
            let elem = (iter.next)(ctx)?;
            ctx.call(func, vec![elem])?;
        }
        Ok(Default::default())
    }
);

fndr_native_func!(
    /// Combines two iterables into a single iterator over pairs of their elements
    zip_func,
    |ctx, list1, list2| {
        expect_iterable!(list1 => iter1);
        expect_iterable!(list2 => iter2);
        let iter1_copy = iter1.clone();
        let iter2_copy = iter2.clone();
        Ok(FenderIterator {
            next: Rc::new(move |e| {
                let next1 = (iter1.next)(e)?;
                let next2 = (iter2.next)(e)?;
                let pair = FenderValue::make_list(vec![next1, next2]);
                Ok(pair.into())
            }),
            has_next: Rc::new(move |e| {
                let has_next = *(iter1_copy.has_next)(e)? == FenderValue::Bool(true);
                let has_next2 = *(iter2_copy.has_next)(e)? == FenderValue::Bool(true);
                Ok(FenderValue::Bool(has_next && has_next2).into())
            }),
        }
        .into())
    }
);

fndr_native_func!(
    /// Limit the number of elements an iterator will process
    take_func,
    |ctx, list, limit| {
        use FenderValue::*;
        expect_iterable!(list => iter);
        let limit = type_match!(limit {(Int(i)) => i});
        let iter2 = iter.clone();
        let i = Rc::new(RefCell::new(0));
        let i2 = i.clone();
        Ok(FenderIterator {
            next: Rc::new(move |e| {
                if *i.borrow() >= limit {
                    Ok(Default::default())
                } else {
                    *i.borrow_mut() += 1;
                    (iter.next)(e)
                }
            }),
            has_next: Rc::new(move |e| {
                let has_next = *i2.borrow() < limit && *(iter2.has_next)(e)? == Bool(true);
                Ok(FenderValue::Bool(has_next).into())
            }),
        }
        .into())
    }
);

fndr_native_func!(
    /// Throw out the first n elements
    skip_func,
    |ctx, list, n| {
        use FenderValue::*;
        expect_iterable!(list => iter);
        let n = type_match!(n {(Int(n)) => n});
        for _ in 0..n {
            (iter.next)(ctx)?;
        }
        Ok(iter.into())
    }
);

fndr_native_func!(
    /// Join all elements in an iterable type to a string
    join_func,
    |ctx, list, delim| {
        use FenderValue::*;
        expect_iterable!(list => iter);
        let delim = type_match!(delim {
            (String(s)) => s.deref().clone(),
            (Char(c)) => c.to_string(),
            (Null) => "".to_string()
        });
        let mut buf = std::string::String::new();
        let mut first = true;
        while *(iter.has_next)(ctx)? == Bool(true) {
            if !first {
                buf.push_str(&delim);
            }
            first = false;
            let elem = (iter.next)(ctx)?;
            match &*elem {
                String(s) => buf.push_str(s),
                Char(c) => buf.push(*c),
                _ => buf.push_str(&elem.to_string()),
            }
        }
        Ok(FenderValue::make_string(buf).into())
    }
);

fndr_native_func!(
    /// Counts the elements in an iterator
    count_func,
    |ctx, list| {
        expect_iterable!(list => iter);
        let mut count = 0;
        while *(iter.has_next)(ctx)? == FenderValue::Bool(true) {
            (iter.next)(ctx)?;
            count += 1;
        }
        Ok(FenderValue::Int(count).into())
    }
);

fndr_native_func!(
    /// Maps the elements in an iterator using a function that takes both the index and element
    map_indexed_func,
    |ctx, list, func| {
        expect_iterable!(list => iter);
        expect_func!(func => func);
        let func = func.clone();
        let i = Rc::new(RefCell::new(0));
        Ok(FenderIterator {
            next: Rc::new(move |e| {
                let elem = (iter.next)(e)?;
                let mut borrow = i.borrow_mut();
                let val = e.call(&func, vec![FenderValue::Int(*borrow).into(), elem])?;
                *borrow += 1;
                Ok(val)
            }),
            has_next: iter.has_next.clone(),
        }
        .into())
    }
);

fndr_native_func!(
    /// Splits a string given a certain substring or character
    split_func,
    |ctx, str, delim| {
        use FenderValue::*;
        type_match!(str, delim {
            (String(str), String(delim)) => {
                let list = str
                    .split(&**delim)
                    .map(|v| FenderValue::make_string(v.to_string()).into())
                    .collect();
                Ok(FenderValue::make_list(list).into())
            },
            (String(str), Char(c)) => {
                let list = str
                    .split(c)
                    .map(|v| FenderValue::make_string(v.to_string()).into())
                    .collect();
                Ok(FenderValue::make_list(list).into())
            }
        })
    }
);

fndr_native_func!(
    /// Converts a string to uppercase
    upper_func,
    |ctx, str| {
        use FenderValue::*;
        type_match!(str {
            (String(s)) => Ok(FenderValue::make_string(s.to_uppercase()).into()),
            (Char(c)) => Ok(FenderValue::Char(c.to_ascii_uppercase()).into())
        })
    }
);

fndr_native_func!(
    /// Converts a string to lowercase
    lower_func,
    |ctx, str| {
        use FenderValue::*;
        type_match!(str {
            (String(s)) => Ok(FenderValue::make_string(s.to_lowercase()).into()),
            (Char(c)) => Ok(FenderValue::Char(c.to_ascii_lowercase()).into())
        })
    }
);

fndr_native_func!(
    /// Calculates the sum of an iterable
    sum_func,
    |ctx, list| {
        use FenderValue::*;
        expect_iterable!(list => iter);
        let mut sum = Int(0).into();
        while *(iter.has_next)(ctx)? == Bool(true) {
            let elem = (iter.next)(ctx)?;
            sum = FenderBinaryOperator::Add.apply_2(&sum, &elem);
        }
        Ok(sum)
    }
);

fndr_native_func!(
    /// Calculates the sum of an iterable
    product_func,
    |ctx, list| {
        use FenderValue::*;
        expect_iterable!(list => iter);
        let mut sum = Int(1).into();
        while *(iter.has_next)(ctx)? == Bool(true) {
            let elem = (iter.next)(ctx)?;
            sum = FenderBinaryOperator::Mul.apply_2(&sum, &elem);
        }
        Ok(sum)
    }
);

fndr_native_func!(
    /// Remove elements that return `false` when passed to `pred`
    take_while_func,
    |_, list, pred| {
        expect_iterable!(list => iter);
        expect_func!(pred => pred);
        let next: Rc<RefCell<Option<FenderReference>>> = Rc::new(RefCell::new(None));
        let next2 = next.clone();
        let iter2 = iter;
        let pred2 = pred.clone();
        Ok(FenderIterator {
            next: Rc::new(move |e| {
                if let Some(val) = std::mem::take(&mut *next.borrow_mut()) {
                    Ok(val)
                } else {
                    Ok(Default::default())
                }
            }),
            has_next: Rc::new(move |e| {
                if next2.borrow().is_some() {
                    Ok(FenderValue::Bool(true).into())
                } else {
                    if *(iter2.has_next)(e)? == FenderValue::Bool(true) {
                        let elem = (iter2.next)(e)?;
                        if e.call(&pred2, vec![elem.clone()])?.to_bool() == FenderValue::Bool(true)
                        {
                            next2.replace(Some(elem));
                        } else {
                            return Ok(FenderValue::Bool(false).into());
                        }
                    }
                    Ok(FenderValue::Bool(next2.borrow().is_some()).into())
                }
            }),
        }
        .into())
    }
);
