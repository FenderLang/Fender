use freight_vm::value::Value;

use crate::{
    create_default_struct_constructor, fender_reference::InternalReference,
    fender_value::FenderValue, fndr_native_func, operators::field_access,
    stdlib::iterators::iter_result_constructor,
};

create_default_struct_constructor!(
    list_iter_constructor, ListIter, {next_func, inner_list, pos}
);

fndr_native_func!(
    /// next value in list
    list_iter_next_func,
    |ctx, mut list_iter| {
        const ERROR_MSG: &str = "INVALID LIST NEXT CALL";

        let FenderValue::Struct(iter) = &mut *list_iter else{return Ok(FenderValue::make_error(ERROR_MSG).into())};

        let type_id = iter.struct_id.clone();

        let mut pos_ref = (*iter.data.get(&(type_id.fields[2].2 as i64)).unwrap()).dupe_ref();
        let FenderValue::Int(pos_val) = pos_ref.unwrap_value()else{todo!()};

        let inner_list_ref = (*iter.data.get(&(type_id.fields[1].2 as i64)).unwrap()).dupe_ref();
        let FenderValue::List(inner_list_val) = inner_list_ref.unwrap_value()else{todo!()};

        if !((pos_val as usize) < inner_list_val.len()) {
            return iter_result_constructor(
                ctx,
                vec![FenderValue::Bool(false).into(), FenderValue::Null.into()],
            );
        }

        let ret = inner_list_val[pos_val as usize].dupe_ref();

        match &mut *pos_ref {
            FenderValue::Int(i) => *i += 1,
            _ => unreachable!(),
        }

        iter.data.insert(
            (type_id.fields[2].2 as i64),
            InternalReference::new(FenderValue::Int(pos_val + 1).into()),
        );

        iter_result_constructor(ctx, vec![FenderValue::Bool(true).into(), ret])
    }
);
