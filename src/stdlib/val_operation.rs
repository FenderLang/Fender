use crate::{
    fndr_native_func, type_match,
    type_sys::{
        fender_value::FenderValue::{self, *},
        type_id::FenderTypeId,
    },
};

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
        let (pos_a, pos_b) = type_match!(
            pos_a, pos_b {
                (Int(pos_a), Int(pos_b))=> (pos_a, pos_b)
            }
        );

        type_match! {
            variable
            {
                List(mut l) => {
                    l.swap(pos_a as usize, pos_b as usize);
                },
                String(mut s) => {
                    s.swap(pos_a as usize, pos_b as usize);
                }
            }
        };

        Ok(variable)
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

fndr_native_func!(
    /// Returns a `String` that contains the debug text of the given `value`
    dbg_func,
    |_, value| { Ok(FenderValue::make_string(value.fender_dbg_string()).into()) }
);

fndr_native_func!(
    /// Removes an element from a list and returns it or error if there is no element at that location
    remove_func,
    |_, mut value, pos| {
        let pos = type_match! {pos {Int(pos) => pos}};

        Ok(match value.remove_at(pos) {
            Ok(v) => v,
            Err(s) => FenderValue::make_error(s).into(),
        })
    }
);

fndr_native_func!(
    /// Removes an element from a list and returns the list it was removed from
    remove_pass_func,
    |_, mut value, pos| {
        let Int(pos) = *pos else {
            return Ok(FenderValue::make_error(format!("Remove must be indexed with an int: expected type `Int` found type `{}`", pos.get_type_id().to_string())).into());
        };
        Ok(match value.remove_at(pos) {
            Ok(_) => value,
            Err(s) => FenderValue::make_error(s).into(),
        })
    }
);

fndr_native_func!(
    /// Returns and String/List with the combined String/List
    concat_func,
    |_, a, b| {
        type_match!(
            a, b {
                (String(a), String(b)) => Ok(FenderValue::make_string(format!("{}{}", *a, *b)).into()),
                (List(a), List(b)) => {
                    let mut new_list = a.to_vec();
                    new_list.extend(b.iter().cloned());
                    Ok(FenderValue::make_list(new_list).into())
                }
            }
        )
    }
);

fndr_native_func!(
    /// Insert a key-value pair into a HashMap, or insert into a list at a given index
    insert_func,
    |_, mut collection, key, value| {
        let res = match collection.get_real_type_id() {
            FenderTypeId::HashMap => (*collection).insert(key.unwrap_value(), value),
            FenderTypeId::List if key.get_real_type_id() == FenderTypeId::Int => {
                collection.insert(key.unwrap_value(), value)
            }
            FenderTypeId::List => Err(format!(
                "Cannot index `List` with type `{}`",
                key.get_type_id().to_string()
            )),
            t => Err(format!("Cannot call `insert` on type `{}`", t.to_string())),
        };

        Ok(match res {
            Ok(v) => v,
            Err(e) => FenderValue::make_error(e).into(),
        })
    }
);

fndr_native_func!(
    /// Trim leading and trailing characters that match `ignore_list` or `DEFAULT_TRIM_SET`
    ///
    /// this modifies incoming `string` and returns it
    trim_func,
    |_, mut string, ignore_list| {
        const DEFAULT_TRIM_SET: &[char] = &['\t', ' ', '\n', '\r'];

        type_match!(
            string, ignore_list {
            (String(mut s), List(mut l)) => {

                let char_list = l.iter().filter_map(|v| v.to_string().chars().next()).collect::<Vec<_>>();
                *s = s.to_string().trim_matches(&char_list[..]).into();
            },
            (String(mut s), Char(c)) => *s = s.to_string().trim_matches(c).into(),
            (String(mut s), String(ignore_s)) => *s = s.to_string().trim_matches(&ignore_s.chars()[..]).into(),
            (String(mut s), Null) => *s = s.to_string().trim_matches(DEFAULT_TRIM_SET).into()

        });

        Ok(string)
    }
);
fndr_native_func!(
    /// Trim leading and trailing characters that match `ignore_list` or `DEFAULT_TRIM_SET`
    get_trimmed_func,
    |_, mut string, ignore_list| {
        const DEFAULT_TRIM_SET: &[char] = &['\t', ' ', '\n', '\r'];

        let string = type_match!(
            string, ignore_list {
            (String(s), List(mut l)) => {

                let char_list = l.iter().filter_map(|v| v.to_string().chars().next()).collect::<Vec<_>>();
                s.to_string().trim_matches(&char_list[..]).to_owned()
            },
            (String(s), Char(c)) => s.to_string().trim_matches(c).into(),
            (String(s), String(ignore_s)) => s.to_string().trim_matches(&ignore_s.chars()[..]).to_owned(),
            (String(s), Null) => s.to_string().trim_matches(DEFAULT_TRIM_SET).to_owned()

        });

        Ok(FenderValue::make_string(string).into())
    }
);
