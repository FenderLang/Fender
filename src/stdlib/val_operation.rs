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
