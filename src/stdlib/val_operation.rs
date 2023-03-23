use crate::{
    fender_value::FenderValue::{self, *},
    fndr_native_func,
};
use std::ops::DerefMut;

fndr_native_func!(
    /// Get the `FenderValue::len` of `item`
    len_func,
    |_, item| {
        Ok(match item.len() {
            Ok(len) => Int(len as i64),
            Err(e_str) => Error(e_str),
        }
        .into())
    }
);

fndr_native_func!(
    /// Swap two values in a list
    swap_func,
    |_, mut variable, pos_a, pos_b| {
        let (Int(pos_a), Int(pos_b)) =  (&*pos_a, &*pos_b) else{
        return Ok(FenderValue::make_error(format!(
            "Swap indices must be of type Int, values provided were of following types ({:?}, {:?})",
            pos_a.get_real_type_id(),
            pos_b.get_real_type_id(),
        ))
        .into());
    };

        Ok(match variable.deref_mut() {
            List(l) => {
                l.swap(*pos_a as usize, *pos_b as usize);
                List(l.to_vec()).into()
            }

            _ => Error(format!(
                "Cannot call swap on value of type {:?}",
                variable.get_real_type_id()
            ))
            .into(),
        })
    }
);

fndr_native_func!(
    /// Shuffles a list in place, also returns the resulting list
    shuffle_func,
    |_, mut list| {
        Ok(match list.shuffle() {
            Ok(v) => list,
            Err(e) => FenderValue::make_error(e).into(),
        })
    }
);

fndr_native_func!(
    /// Creates, and returns, a copy of the given list shuffled,
    /// without modifying the original list
    get_shuffled_func,
    |_, mut list| {
        Ok(match list.get_shuffled() {
            Ok(v) => v.into(),
            Err(e) => FenderValue::make_error(e).into(),
        })
    }
);

fndr_native_func!(
    /// Get random float between 0 and 1
    rand_func,
    |_| { Ok(FenderValue::Float(rand::random()).into()) }
);

fndr_native_func!(
    /// Pushes a value to the end of a list
    push_func,
    |_, mut list, value| {
        Ok(match list.push(value) {
            Ok(_) => list,
            Err(e) => FenderValue::make_error(e).into(),
        })
    }
);

fndr_native_func!(
    /// Pops a value off the end of a list
    pop_func,
    |_, mut list| {
        Ok(match list.pop() {
            Ok(v) => v,
            Err(e) => FenderValue::make_error(e).into(),
        })
    }
);
