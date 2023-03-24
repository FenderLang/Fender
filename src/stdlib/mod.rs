#![deny(missing_docs)]
use crate::{fender_value::FenderValue, type_sys::type_system::FenderTypeSystem};
use flux_bnf::tokens::{iterators::SelectTokens, Token};
use freight_vm::{
    expression::{Expression, NativeFunction},
    function::{ArgCount, FunctionWriter},
    vm_writer::VMWriter,
};
use std::{
    collections::{HashMap, HashSet},
    ops::RangeBounds,
};

/// functions for converting fender values
pub mod cast;
/// conditional execution and branching
pub mod control_flow;
/// stdin, stdout, and file IO
pub mod io;
/// system interface
pub mod system;
/// modify and query existing values
pub mod val_operation;

/// Detect which standard library functions are used and load them automatically
pub fn detect_load(
    token: &Token,
    globals: &mut HashMap<String, usize>,
    main: &mut FunctionWriter<FenderTypeSystem>,
    vm: &mut VMWriter<FenderTypeSystem>,
) {
    let names: HashSet<_> = token
        .rec_iter()
        .select_token("name")
        .map(|t| t.get_match())
        .collect();
    for name in names {
        let function = get_stdlib_function(&name);
        let Some((function, args)) = function else { continue };

        let native = vm.include_native_function(function, args);
        let global = vm.create_global();
        globals.insert(name, global);
        main.evaluate_expression(Expression::AssignGlobal(
            global,
            Box::new(FenderValue::Function(native).into()),
        ));
    }
}

/// Shorthand function to create fixed `ArgCount`
fn fixed(args: usize) -> ArgCount {
    ArgCount::Fixed(args)
}
/// Shorthand function to create ranged `ArgCount`
fn range<RB: RangeBounds<usize>>(args: RB) -> ArgCount {
    ArgCount::new(args)
}
/// Get the native rust function to be called when `name` is called in fender
pub fn get_stdlib_function(name: &str) -> Option<(NativeFunction<FenderTypeSystem>, ArgCount)> {
    Some(match name {
        "print" => (NativeFunction::new(io::print_func), range(1..)),
        "println" => (NativeFunction::new(io::println_func), range(1..)),
        "readLine" => (NativeFunction::new(io::read_line_func), fixed(0)),
        "read" => (NativeFunction::new(io::read_func), fixed(1)),
        "write" => (NativeFunction::new(io::write_func), fixed(2)),
        "append" => (NativeFunction::new(io::append_func), fixed(2)),

        "raw" => (NativeFunction::new(cast::get_raw_func), fixed(1)),
        "int" => (NativeFunction::new(cast::int_func), fixed(1)),
        "str" => (NativeFunction::new(cast::str_func), fixed(1)),
        "bool" => (NativeFunction::new(cast::to_bool_func), fixed(1)),
        "ref" => (NativeFunction::new(cast::to_ref_func), fixed(1)),
        "list" => (NativeFunction::new(cast::to_list_func), fixed(1)),
        "joinStr" => (NativeFunction::new(cast::join_to_string_func), fixed(1)),

        "if" => (NativeFunction::new(control_flow::if_func), fixed(2)),
        "else" => (NativeFunction::new(control_flow::else_func), fixed(2)),
        "then" => (NativeFunction::new(control_flow::then_func), fixed(2)),
        "while" => (NativeFunction::new(control_flow::while_func), fixed(2)),
        "also" => (NativeFunction::new(control_flow::also_func), fixed(2)),
        "apply" => (NativeFunction::new(control_flow::apply_func), fixed(2)),

        "len" => (NativeFunction::new(val_operation::len_func), fixed(1)),
        "swap" => (NativeFunction::new(val_operation::swap_func), fixed(3)),
        "shuffle" => (NativeFunction::new(val_operation::shuffle_func), fixed(1)),
        "getShuffled" => (
            NativeFunction::new(val_operation::get_shuffled_func),
            fixed(1),
        ),
        "rand" => (NativeFunction::new(val_operation::rand_func), fixed(0)),
        "push" => (NativeFunction::new(val_operation::push_func), fixed(2)),
        "pop" => (NativeFunction::new(val_operation::pop_func), fixed(1)),
        "dbg" => (NativeFunction::new(val_operation::dbg_func), fixed(1)),
        "remove" => (NativeFunction::new(val_operation::remove_func), fixed(2)),
        "removePass" => (
            NativeFunction::new(val_operation::remove_pass_func),
            fixed(2),
        ),

        "shell" => (NativeFunction::new(system::shell_func), range(1..=3)),

        _ => return None,
    })
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

#[macro_export]
/// Create fender function in rust
macro_rules! fndr_native_func {
    (
        $(#[$docs:meta])*
        $name:ident, | $ctx:tt $(, $($arg:pat_param),*)? | $body:expr
    ) => {
        $(#[$docs])*
        #[allow(unused)]
        pub fn $name(
            $ctx: &mut freight_vm::execution_engine::ExecutionEngine<
                $crate::type_sys::type_system::FenderTypeSystem
            >,
            args: Vec<$crate::fender_reference::FenderReference>,
        ) -> Result<$crate::fender_reference::FenderReference, freight_vm::error::FreightError> {
            const _ARG_COUNT: usize = $crate::count!($($($arg),*)?);
            $(
                    let [$($arg),*]: [$crate::fender_reference::FenderReference; _ARG_COUNT]  = args.try_into().unwrap();
                    )?
            $body
        }
    }
}
