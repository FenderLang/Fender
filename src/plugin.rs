use crate::{fender_value::FenderValue, type_sys::type_system::FenderTypeSystem};
use freight_vm::{expression::NativeFunction, function::ArgCount};
use libloading::{Library, Symbol};
use std::cell::RefCell;
use std::fmt::Debug;
use std::rc::Rc;
use std::{collections::HashMap, ffi::OsStr};

pub trait Plugin: Debug {
    fn name(&self) -> &'static str;
    fn on_plugin_load(&self) {}
    fn get_values(&self) -> HashMap<&str, FenderValue>;
    fn get_functions(&self) -> HashMap<&str, (&NativeFunction<FenderTypeSystem>, ArgCount)>;
}

#[macro_export]
macro_rules! declare_plugin {
    ($plugin_type:ty, $constructor:path) => {
        #[no_mangle]
        pub extern "C" fn _plugin_create() -> *mut dyn $crate::plugin::Plugin {
            let constructor: fn() -> $plugin_type = $constructor;

            let object = constructor();
            let boxed: Box<dyn $crate::plugin::Plugin> = Box::new(object);
            Box::into_raw(boxed)
        }
    };
}

#[derive(Default, Debug)]
pub struct PluginManager {
    plugins: Rc<RefCell<Vec<Box<dyn Plugin>>>>,
    loaded_libraries: Vec<Library>,
}

impl PluginManager {
    pub fn plugins(&self) -> Rc<RefCell<Vec<Box<dyn Plugin>>>> {
        self.plugins.clone()
    }

    pub unsafe fn load_plugin<P: AsRef<OsStr>>(&mut self, filename: P) -> Result<(), ()> {
        type PluginCreate = unsafe fn() -> *mut dyn Plugin;

        let lib = Library::new(filename.as_ref()).unwrap();

        self.loaded_libraries.push(lib);

        let lib = self.loaded_libraries.last().unwrap();

        let constructor: Symbol<PluginCreate> = lib.get(b"_plugin_create").unwrap();
        let boxed_raw = constructor();

        let plugin = Box::from_raw(boxed_raw);
        plugin.on_plugin_load();
        self.plugins.borrow_mut().push(plugin);

        Ok(())
    }
}
