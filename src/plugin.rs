use crate::type_sys::{fender_value::FenderValue, freight_type_system::FenderTypeSystem};
use freight_vm::{expression::NativeFunction, function::ArgCount};
use libloading::{Library, Symbol};
use std::error::Error;
use std::fmt::Debug;
use std::{collections::HashMap, ffi::OsStr};

pub type PluginConstructorType = unsafe fn() -> *mut dyn Plugin;
pub type FenderPluginFunction = (NativeFunction<FenderTypeSystem>, ArgCount);

pub trait Plugin: Debug {
    fn name(&self) -> &'static str;
    fn on_plugin_load(&self) {}
    fn get_values(&self) -> HashMap<&str, &FenderValue>;
    fn get_functions(&self) -> HashMap<&str, &FenderPluginFunction>;
}

#[macro_export]
macro_rules! declare_plugin {
    ($plugin_type:ty, $constructor:path) => {
        #[no_mangle]
        pub fn __plugin_constructor() -> *mut dyn $crate::plugin::Plugin {
            let constructor: fn() -> $plugin_type = $constructor;

            let object = constructor();
            let boxed: Box<dyn $crate::plugin::Plugin> = Box::new(object);
            Box::into_raw(boxed)
        }
    };
}

#[derive(Default, Debug)]
pub struct PluginManager {
    plugins: Vec<Box<dyn Plugin>>,
    loaded_libraries: Vec<Library>,
}

impl PluginManager {
    pub fn plugins(&self) -> &Vec<Box<dyn Plugin>> {
        &self.plugins
    }

    /// # Safety
    ///
    /// This function should only be called on a dynamic library using rust calling convention
    pub unsafe fn load_plugin<P: AsRef<OsStr>>(
        &mut self,
        filename: P,
    ) -> Result<(), Box<dyn Error>> {
        self.loaded_libraries.push(Library::new(filename.as_ref())?);
        let lib = self.loaded_libraries.last().unwrap();

        let constructor: Symbol<PluginConstructorType> = lib.get(b"__plugin_constructor").unwrap();

        let plugin = Box::from_raw(constructor());
        plugin.on_plugin_load();
        self.plugins.push(plugin);

        Ok(())
    }
}
