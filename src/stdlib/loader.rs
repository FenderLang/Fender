use std::collections::HashMap;

use crate::{stdlib, FenderValue};
use flux_bnf::tokens::{iterators::SelectTokens, Token};
use freight_vm::{
    expression::{Expression, NativeFunction},
    function::FunctionWriter,
    vm_writer::VMWriter,
};

use crate::FenderTypeSystem;

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
        "print" => (NativeFunction::new(stdlib::print_func), 1),
        "println" => (NativeFunction::new(stdlib::println_func), 1),
        "if" => (NativeFunction::new(stdlib::if_func), 3),
        "readLine" => (NativeFunction::new(stdlib::read_line_func), 0),
        "raw" => (NativeFunction::new(stdlib::get_raw_func), 1),
        "len" => (NativeFunction::new(stdlib::len_func), 1),
        "int" => (NativeFunction::new(stdlib::int_func), 1),
        "read" => (NativeFunction::new(stdlib::read_func), 1),
        "write" => (NativeFunction::new(stdlib::write_func), 2),
        _ => return None,
    })
}
