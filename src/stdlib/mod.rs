#![deny(missing_docs)]
use self::{loader::StdlibResource, native_types::functions::FenderNativeFunction};
use crate::{dep_name, deps_enum, fndr_native_struct, type_sys::type_system::FenderTypeSystem};
use freight_vm::{execution_engine::ExecutionEngine, function::ArgCount};
use std::ops::RangeBounds;

/// functions for converting fender values
pub mod cast;
/// conditional execution and branching
pub mod control_flow;
/// stdin, stdout, and file IO
pub mod io;
/// Default iterator types and their functions
pub mod iterators;
/// loads stdlib resources
pub mod loader;
/// native rust types to be loaded by `stdlib::load`
pub mod native_types {
    /// native function type and macros
    pub mod functions;
    /// native struct type and macros
    pub mod structs;
}
/// system interface
pub mod system;
/// modify and query existing values
pub mod val_operation;

/// Load a standard library resource
pub fn load<const N: usize>(
    name: &str,
    engine: &mut ExecutionEngine<FenderTypeSystem>,
) -> Option<usize> {
    let res = FenderResource::from_str(name)?;
    Some(res.load::<N>(engine))
}

/// Shorthand function to create fixed `ArgCount`
fn fixed(args: usize) -> ArgCount {
    ArgCount::Fixed(args)
}

/// Shorthand function to create `ArgCount::Variadic`
fn variadic<RB: RangeBounds<usize>>(args: RB) -> ArgCount {
    ArgCount::new_variadic(args)
}

/// Shorthand function to create `ArgCount::Range`
fn range<RB: RangeBounds<usize>>(args: RB) -> ArgCount {
    ArgCount::new(args)
}

deps_enum! {FenderResource, STDLIB_SIZE:
    print => FenderNativeFunction {func: io::print_func, args: variadic(1..)},
    println => FenderNativeFunction {func: io::println_func, args: variadic(1..)},
    readLine => FenderNativeFunction {func: io::read_line_func, args: fixed(0)},
    read => FenderNativeFunction {func: io::read_func, args: fixed(1)},
    write => FenderNativeFunction {func: io::write_func, args: fixed(2)},
    append => FenderNativeFunction {func: io::append_func, args: fixed(2)},

    raw => FenderNativeFunction {func: cast::get_raw_func, args: fixed(1)},
    int => FenderNativeFunction {func: cast::int_func, args: fixed(1)},
    str => FenderNativeFunction {func: cast::str_func, args: fixed(1)},
    bool => FenderNativeFunction {func: cast::to_bool_func, args: fixed(1)},
    @ "ref" r#ref => FenderNativeFunction {func: cast::to_ref_func, args: fixed(1)},
    list => FenderNativeFunction {func: cast::to_list_func, args: fixed(1)},
    joinStr => FenderNativeFunction {func: cast::join_to_string_func, args: fixed(1)},

    @ "if" r#if => FenderNativeFunction {func: control_flow::if_func, args: fixed(3)},
    @ "else" r#else => FenderNativeFunction {func: control_flow::else_func, args: fixed(2)},
    then => FenderNativeFunction {func: control_flow::then_func, args: fixed(2)},
    @ "while" r#while => FenderNativeFunction {func: control_flow::while_func, args: fixed(2)},
    also => FenderNativeFunction {func: control_flow::also_func, args: fixed(2)},
    apply => FenderNativeFunction {func: control_flow::apply_func, args: fixed(2)},

    len => FenderNativeFunction {func: val_operation::len_func, args: fixed(1)},
    swap => FenderNativeFunction {func: val_operation::swap_func, args: fixed(3)},
    shuffle => FenderNativeFunction {func: val_operation::shuffle_func, args: fixed(1)},
    getShuffled => FenderNativeFunction {func: val_operation::get_shuffled_func, args: fixed(1), },
    rand => FenderNativeFunction {func: val_operation::rand_func, args: fixed(0)},
    push => FenderNativeFunction {func: val_operation::push_func, args: fixed(2)},
    pop => FenderNativeFunction {func: val_operation::pop_func, args: fixed(1)},
    dbg => FenderNativeFunction {func: val_operation::dbg_func, args: fixed(1)},
    remove => FenderNativeFunction {func: val_operation::remove_func, args: fixed(2)},
    removePass => FenderNativeFunction {func: val_operation::remove_pass_func, args: fixed(2), },
    concat => FenderNativeFunction {func: val_operation::concat_func, args: fixed(2)},
    insert => FenderNativeFunction {func: val_operation::insert_func, args: fixed(3)},

    shell => FenderNativeFunction {func: system::shell_func, args: range(1..=3)},
    pwd => FenderNativeFunction {func: system::pwd_func, args: fixed(0)},
    cd => FenderNativeFunction {func: system::cd_func, args: fixed(1)},

    IterRes => fndr_native_struct!(IterRes, {valid: Bool, data: None}, iterators::iter_result_constructor),
    __list_next [IterRes] => FenderNativeFunction {func: iterators::list::list_iter_next_func, args: fixed(1)},
    ListIter[IterRes] => fndr_native_struct!(ListIter, {next_func: Function, inner_list: List, pos: Int}, iterators::list::list_iter_constructor),
    next [IterRes] => FenderNativeFunction {func: iterators::next_func, args: fixed(1)},
    iter [next, ListIter] => FenderNativeFunction {func: iterators::iter_func, args: fixed(1)},
}

#[macro_export]
/// Count number of parameters
macro_rules! count {
    ($first:pat_param, $($rest:pat_param),*) => {
        1 + $crate::count!($($rest),*)
    };
    ($first:pat_param) => {1};
    () => {0};
}
