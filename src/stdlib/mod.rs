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
    function::{ArgCount, FunctionRef, FunctionWriter},
};
use std::{collections::HashMap, ops::RangeBounds};

use self::loader::{DependencyList, StdlibResource};

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
    engine: &mut ExecutionEngine<FenderTypeSystem>,
) -> DependencyList<STDLIB_SIZE> {
    let mut dep_list = DependencyList([None; STDLIB_SIZE]);
    token
        .rec_iter()
        .select_token("name")
        .map(|n| n.get_match())
        .filter_map(|n| FenderResource::from_str(&n))
        .for_each(|res| {
            let global = res.load(engine, main, &mut dep_list);
            globals.insert(res.name().to_string(), global);
        });
    dep_list
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

struct FenderNativeFunction {
    func: fn(
        &mut ExecutionEngine<FenderTypeSystem>,
        Vec<FenderReference>,
    ) -> Result<FenderReference, FreightError>,
    args: ArgCount,
}

impl StdlibResource for FenderNativeFunction {
    fn load_into<const N: usize>(
        &self,
        engine: &mut ExecutionEngine<FenderTypeSystem>,
        main: &mut FunctionWriter<FenderTypeSystem>,
        _: &mut DependencyList<N>,
    ) -> usize {
        let global = engine.create_global();
        let func = FunctionRef::new_native(global, NativeFunction::new(self.func), self.args);
        main.evaluate_expression(Expression::AssignGlobal(
            global,
            Box::new(FenderValue::Function(func).into()),
        ));

        global
    }
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
        shell => FenderNativeFunction {func: system::shell_func, args: range(1..=3)},
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
