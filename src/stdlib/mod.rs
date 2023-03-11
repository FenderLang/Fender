use crate::{fender_value::FenderValue, type_sys::type_system::FenderTypeSystem};
use flux_bnf::tokens::{iterators::SelectTokens, Token};
use freight_vm::{
    expression::{Expression, NativeFunction},
    function::FunctionWriter,
    vm_writer::VMWriter,
};
use std::collections::HashMap;

pub mod cast;
pub mod control_flow;
pub mod io;
pub mod val_operation;

/// Detect which standard library functions are used and load them automatically
pub fn detect_load(
    token: &Token,
    globals: &mut HashMap<String, usize>,
    main: &mut FunctionWriter<FenderTypeSystem>,
    vm: &mut VMWriter<FenderTypeSystem>,
) {
    for name in token.rec_iter().select_token("name") {
        let name = name.get_match();
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
        _ => return None,
    })
}
