#![deny(missing_docs)]
use crate::{
    dep_name, deps_enum, fender_reference::FenderReference, fender_value::FenderValue,
    type_sys::type_system::FenderTypeSystem,
};
use flux_bnf::tokens::{iterators::SelectTokens, Token};
use freight_vm::{
    error::FreightError,
    execution_engine::ExecutionEngine,
    expression::{Expression, NativeFunction},
    function::{ArgCount, FunctionWriter},
    vm_writer::VMWriter,
};
use std::{
    collections::{HashMap, HashSet},
    ops::RangeBounds,
};

use self::{io::print_func, loader::StdlibResource};

/// functions for converting fender values
pub mod cast;
/// conditional execution and branching
pub mod control_flow;
/// stdin, stdout, and file IO
pub mod io;
/// loads stdlib resources
pub mod loader;
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

struct FenderNativeFunction {
    func: fn(
        &mut ExecutionEngine<FenderTypeSystem>,
        Vec<FenderReference>,
    ) -> Result<FenderReference, FreightError>,
    args: ArgCount,
}

impl StdlibResource for FenderNativeFunction {
    fn set_deps<const N: usize>(&self, _: &mut loader::DependencyList<N>) {}

    fn load_into(
        &self,
        writer: &mut VMWriter<FenderTypeSystem>,
        main: &mut FunctionWriter<FenderTypeSystem>,
    ) -> usize {
        let func = writer.include_native_function(NativeFunction::new(self.func), self.args);
        let global = writer.create_global();
        main.evaluate_expression(Expression::AssignGlobal(
            global,
            Box::new(FenderValue::Function(func).into()),
        ));
        global
    }
}
deps_enum! {FenderResource, STDLIB_SIZE:
        print => FenderNativeFunction {func: io::print_func, args: range(1..)},
        println => FenderNativeFunction {func: io::println_func, args: range(1..)},
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
        @ "if" r#if => FenderNativeFunction {func: control_flow::if_func, args: fixed(2)},
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
        shell => FenderNativeFunction {func: system::shell_func, args: range(1..=3)},
}

/// Get the native rust function to be called when `name` is called in fender
pub fn get_stdlib_function(name: &str) -> Option<(NativeFunction<FenderTypeSystem>, ArgCount)> {
    Some(match name {
        // "print" => (NativeFunction::new(io::print_func), range(1..)),
        // "println" => (NativeFunction::new(io::println_func), range(1..)),
        // "readLine" => (NativeFunction::new(io::read_line_func), fixed(0)),
        // "read" => (NativeFunction::new(io::read_func), fixed(1)),
        // "write" => (NativeFunction::new(io::write_func), fixed(2)),
        // "append" => (NativeFunction::new(io::append_func), fixed(2)),
        // "raw" => (NativeFunction::new(cast::get_raw_func), fixed(1)),
        // "int" => (NativeFunction::new(cast::int_func), fixed(1)),
        // "str" => (NativeFunction::new(cast::str_func), fixed(1)),
        // "bool" => (NativeFunction::new(cast::to_bool_func), fixed(1)),
        // "ref" => (NativeFunction::new(cast::to_ref_func), fixed(1)),
        // "list" => (NativeFunction::new(cast::to_list_func), fixed(1)),
        // "joinStr" => (NativeFunction::new(cast::join_to_string_func), fixed(1)),
        // "if" => (NativeFunction::new(control_flow::if_func), fixed(2)),
        // "else" => (NativeFunction::new(control_flow::else_func), fixed(2)),
        // "then" => (NativeFunction::new(control_flow::then_func), fixed(2)),
        // "while" => (NativeFunction::new(control_flow::while_func), fixed(2)),
        // "also" => (NativeFunction::new(control_flow::also_func), fixed(2)),
        // "apply" => (NativeFunction::new(control_flow::apply_func), fixed(2)),
        // "len" => (NativeFunction::new(val_operation::len_func), fixed(1)),
        // "swap" => (NativeFunction::new(val_operation::swap_func), fixed(3)),
        // "shuffle" => (NativeFunction::new(val_operation::shuffle_func), fixed(1)),
        // "getShuffled" => ( NativeFunction::new(val_operation::get_shuffled_func), fixed(1), ),
        // "rand" => (NativeFunction::new(val_operation::rand_func), fixed(0)),
        // "push" => (NativeFunction::new(val_operation::push_func), fixed(2)),
        // "pop" => (NativeFunction::new(val_operation::pop_func), fixed(1)),
        // "dbg" => (NativeFunction::new(val_operation::dbg_func), fixed(1)),
        // "remove" => (NativeFunction::new(val_operation::remove_func), fixed(2)),
        // "removePass" => ( NativeFunction::new(val_operation::remove_pass_func), fixed(2), ),
        // "shell" => (NativeFunction::new(system::shell_func), range(1..=3)),
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
