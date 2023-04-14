use fender::{
    declare_plugin, fender_value::FenderValue, fndr_native_func, plugin::Plugin,
    type_sys::type_system::FenderTypeSystem,
};
use freight_vm::{expression::NativeFunction, function::ArgCount};
use std::collections::HashMap;

#[derive(Debug)]
struct MyPlugin {
    example_1: (NativeFunction<FenderTypeSystem>, ArgCount),
    example_2: (NativeFunction<FenderTypeSystem>, ArgCount),
    name_list: FenderValue,
}

declare_plugin!(MyPlugin, constructor);

fn constructor() -> MyPlugin {
    MyPlugin {
        example_1: (
            NativeFunction::new(example_1_func),
            ArgCount::Variadic { min: 1, max: 1 },
        ),
        example_2: (NativeFunction::new(other_name_for_func), ArgCount::Fixed(0)),
        name_list: FenderValue::make_list(vec![
            FenderValue::make_string("FuzzyNovaGoblin".into()).into(),
            FenderValue::make_string("Redempt".into()).into(),
            FenderValue::make_string("GigaRyno".into()).into(),
        ]),
    }
}

impl Plugin for MyPlugin {
    fn name(&self) -> &'static str {
        "default plugin"
    }

    fn get_values(&self) -> std::collections::HashMap<&str, &FenderValue> {
        let mut ret = HashMap::new();
        ret.insert("name_list", &self.name_list);
        ret
    }

    fn get_functions(&self) -> HashMap<&str, &(NativeFunction<FenderTypeSystem>, ArgCount)> {
        let mut ret = HashMap::new();
        ret.insert("example1", &self.example_1);
        ret.insert("example2", &self.example_2);
        ret
    }
}

fndr_native_func!(
    /// example1
    #[no_mangle]
    example_1_func,
    |_, item, argv| {
        use fender::fender_value::FenderValue::*;
        Ok(FenderValue::make_list(vec![item, Int(argv.len().unwrap() as i64).into()]).into())
    }
);

fndr_native_func!(
    /// example2
    #[no_mangle]
    other_name_for_func,
    |_| {
        use fender::fender_value::FenderValue::*;
        Ok(FenderValue::make_string("this is func 2".into()).into())
    }
);


fn main() {
    println!("this example is not meant to be run, it is a look at what a fender plugin might look like")
}
