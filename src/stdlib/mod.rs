#![deny(missing_docs)]
use crate::{
    dep_name, deps_enum,
    fender_reference::FenderReference,
    fender_value::FenderValue,
    type_sys::{type_id::FenderTypeId, type_system::FenderTypeSystem},
};

use freight_vm::{
    error::FreightError,
    execution_engine::ExecutionEngine,
    expression::{Expression, NativeFunction},
    function::{ArgCount, FunctionRef},
};
use std::ops::RangeBounds;

use self::loader::StdlibResource;

/// functions for converting fender values
pub mod cast;
/// conditional execution and branching
pub mod control_flow;
/// functions for manipulating data structures
pub mod data;
/// stdin, stdout, and file IO
pub mod io;
/// loads stdlib resources
pub mod loader;
/// system interface
pub mod system;
/// functions for working with raw `FenderValue::Type`
pub mod types;
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

struct FenderNativeFunction {
    func: fn(
        &mut ExecutionEngine<FenderTypeSystem>,
        Vec<FenderReference>,
    ) -> Result<FenderReference, FreightError>,
    args: ArgCount,
}

impl StdlibResource for FenderNativeFunction {
    fn load_into<const N: usize>(&self, engine: &mut ExecutionEngine<FenderTypeSystem>) -> usize {
        let global = engine.create_global();
        let func = FunctionRef::new_native(global, NativeFunction::new(self.func), self.args);
        engine
            .evaluate(
                &Expression::AssignGlobal(global, Box::new(FenderValue::Function(func).into())),
                &mut [],
                &[],
            )
            .expect("Assigning value should never fail");
        global
    }
}

struct FenderNativeTypeValue {
    pub type_id: FenderTypeId,
}

impl StdlibResource for FenderNativeTypeValue {
    fn load_into<const N: usize>(&self, engine: &mut ExecutionEngine<FenderTypeSystem>) -> usize {
        let global = engine.create_global();
        engine
            .evaluate(
                &Expression::AssignGlobal(
                    global,
                    Box::new(FenderValue::Type(self.type_id.clone()).into()),
                ),
                &mut [],
                &[],
            )
            .expect("Assigning value should never fail");
        global
    }
}

deps_enum! {FenderResource, STDLIB_SIZE:
        print => FenderNativeFunction {func: io::print_func, args: variadic(1..)},
        println => FenderNativeFunction {func: io::println_func, args: variadic(1..)},
        eprint => FenderNativeFunction {func: io::eprint_func, args: variadic(1..)},
        eprintln => FenderNativeFunction {func: io::eprintln_func, args: variadic(1..)},
        readLine => FenderNativeFunction {func: io::read_line_func, args: fixed(0)},
        read => FenderNativeFunction {func: io::read_func, args: fixed(1)},
        write => FenderNativeFunction {func: io::write_func, args: fixed(2)},
        append => FenderNativeFunction {func: io::append_func, args: fixed(2)},

        raw => FenderNativeFunction {func: cast::get_raw_func, args: fixed(1)},
        int => FenderNativeFunction {func: cast::int_func, args: fixed(1)},
        float => FenderNativeFunction {func: cast::float_func, args: fixed(1)},
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
        onErr => FenderNativeFunction {func: control_flow::on_err_func, args: fixed(2)},
        onNull => FenderNativeFunction {func: control_flow::on_null_func, args: fixed(2)},
        onType => FenderNativeFunction {func: control_flow::on_type_func, args: fixed(3)},

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
        trim => FenderNativeFunction {func: val_operation::trim_func, args: range(1..=2)},
        trimmed => FenderNativeFunction {func: val_operation::get_trimmed_func, args: range(1..=2)},

        Int => FenderNativeTypeValue{type_id: FenderTypeId::Int},
        Float => FenderNativeTypeValue{type_id: FenderTypeId::Float},
        Bool => FenderNativeTypeValue{type_id: FenderTypeId::Bool},
        String => FenderNativeTypeValue{type_id: FenderTypeId::String},
        Error => FenderNativeTypeValue{type_id: FenderTypeId::Error},
        Null => FenderNativeTypeValue{type_id: FenderTypeId::Null},
        Reference => FenderNativeTypeValue{type_id: FenderTypeId::Reference},
        Function => FenderNativeTypeValue{type_id: FenderTypeId::Function},
        List => FenderNativeTypeValue{type_id: FenderTypeId::List},
        Char => FenderNativeTypeValue{type_id: FenderTypeId::Char},
        Struct => FenderNativeTypeValue{type_id: FenderTypeId::Struct},
        Type => FenderNativeTypeValue{type_id: FenderTypeId::Type},
        HashMap => FenderNativeTypeValue{type_id: FenderTypeId::HashMap},


        shell => FenderNativeFunction {func: system::shell_func, args: range(1..=3)},
        pwd => FenderNativeFunction {func: system::pwd_func, args: fixed(0)},
        cd => FenderNativeFunction {func: system::cd_func, args: fixed(1)},

        @ "type" r#type => FenderNativeFunction{func: types::get_type_func, args: fixed(1)},
        strToType => FenderNativeFunction{func: types::type_from_name_func, args: fixed(1)},
        isRef => FenderNativeFunction{func: types::is_ref_func, args: fixed(1)},

    map => FenderNativeFunction {func: data::map_func, args: fixed(2)},
    filter => FenderNativeFunction {func: data::filter_func, args: fixed(2)},
    reduce => FenderNativeFunction {func: data::reduce_func, args: fixed(3)},
    each => FenderNativeFunction {func: data::each_func, args: fixed(2)},
    zip => FenderNativeFunction {func: data::zip_func, args: fixed(2)},
    take => FenderNativeFunction {func: data::take_func, args: fixed(2)},
    drop => FenderNativeFunction {func: data::drop_func, args: fixed(2)},
    join => FenderNativeFunction {func: data::join_func, args: range(1..=2)},
    scan => FenderNativeFunction {func: data::scan_func, args: fixed(3)},
    count => FenderNativeFunction {func: data::count_func, args: fixed(1)},
    mapi => FenderNativeFunction {func: data::map_indexed_func, args: fixed(2)},
    takeWhile => FenderNativeFunction {func: data::take_while_func, args: fixed(2)},

    split => FenderNativeFunction {func: data::split_func, args: fixed(2)},
    upper => FenderNativeFunction {func: data::upper_func, args: fixed(1)},
    lower => FenderNativeFunction {func: data::lower_func, args: fixed(1)},
    sum => FenderNativeFunction {func: data::sum_func, args: fixed(1)},
    product => FenderNativeFunction {func: data::product_func, args: fixed(1)},
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
/// Match arguments against their types and generate a nice error message for invalid argument types
macro_rules! type_match {
    ($($arg:ident),* {
        $(
            ($($ty:ident $( ( $($param:pat),* ) )?),*) => $branch:expr
        ),*
        $(,)?
    }) => {
        match ($($arg.unwrap_value()),*) {
            $(( $($ty $( ( $($param),* ) )?),* ) => $branch),* ,
            _ => return Ok($crate::fender_reference::FenderReference::FRaw(FenderValue::make_error(format!("Invalid argument types {}; expected {}",
            format!("({})", [$($arg),*].into_iter().map(|a| a.get_real_type_id().to_string()).collect::<Vec<_>>().join(", ")),
            [
                $( format!("({})", [$(stringify!($ty)),*].join(", ")) ),*
            ].join(" OR "))))),
        }
    };

    ($arg:ident {
        $($ty:ident $( ( $($param:pat),* ) )? => $branch:expr),*
    }) => {
        type_match!($arg {$(($ty $(($($param),*))? ) => $branch),* })
    };
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
