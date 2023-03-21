#![deny(missing_docs)]
use crate::{fender_value::FenderValue, type_sys::type_system::FenderTypeSystem};
use flux_bnf::tokens::{iterators::SelectTokens, Token};
use freight_vm::{
    expression::{Expression, NativeFunction},
    function::FunctionWriter,
    vm_writer::VMWriter,
};
use std::collections::{HashMap, HashSet};

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

/// Get the native rust function to be called when `name` is called in fender
pub fn get_stdlib_function(name: &str) -> Option<(NativeFunction<FenderTypeSystem>, usize)> {
    Some(match name {
        "print" => (NativeFunction::new(io::print_func), 1),
        "println" => (NativeFunction::new(io::println_func), 1),
        "if" => (NativeFunction::new(control_flow::if_func), 3),
        "readLine" => (NativeFunction::new(io::read_line_func), 0),
        "raw" => (NativeFunction::new(cast::get_raw_func), 1),
        "len" => (NativeFunction::new(val_operation::len_func), 1),
        "int" => (NativeFunction::new(cast::int_func), 1),
        "read" => (NativeFunction::new(io::read_func), 1),
        "write" => (NativeFunction::new(io::write_func), 2),
        "swap" => (NativeFunction::new(val_operation::swap_func), 3),
        "str" => (NativeFunction::new(cast::str_func), 1),
        "else" => (NativeFunction::new(control_flow::else_func), 2),
        "then" => (NativeFunction::new(control_flow::then_func), 2),
        "while" => (NativeFunction::new(control_flow::while_func), 2),
        "shell" => (NativeFunction::new(system::shell_func), 3),
        "shuffle" => (NativeFunction::new(val_operation::shuffle_func), 1),
        "getShuffled" => (NativeFunction::new(val_operation::get_shuffled_func), 1),
        "append" => (NativeFunction::new(io::append_func), 2),
        "rand" => (NativeFunction::new(val_operation::rand_func), 0),
        _ => return None,
    })
}
