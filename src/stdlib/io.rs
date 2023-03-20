use crate::{
    fender_value::FenderValue::{self, *},
    fndr_native_func,
};
use std::io::Write;

// --- stdout ---

fndr_native_func!(
    /// Prints to stdout
    print_func,
    |_, item| {
        print!("{}", item.to_string());
        let mut lock = std::io::stdout().lock();
        let _ = lock.flush();
        Ok(Default::default())
    }
);

fndr_native_func!(
    /// Prints to stdout and adds a newline
    println_func,
    |_, item| {
        println!("{}", item.to_string());
        Ok(Default::default())
    }
);

// --- stdin ---

fndr_native_func!(
    /// Read a single line for stdin
    read_line_func,
    |_| {
        match std::io::stdin().lines().next() {
            Some(Ok(s)) => Ok(String(s).into()),
            Some(Err(e)) => Ok(FenderValue::make_error(e.to_string()).into()),
            None => Ok(FenderValue::make_error("End of file".to_string()).into()),
        }
    }
);

// --- file I/O ---

fndr_native_func!(
    /// read in the contents of a file with given path
    read_func,
    |_, file_name| {
        let String(file_name) = &*file_name else {
        return Ok(FenderValue::make_error("file name must be of type `String`").into());
    };
        Ok(match std::fs::read_to_string(file_name) {
            Ok(s) => String(s).into(),
            Err(e) => {
                FenderValue::make_error(format!("failed to read file due to error: {e}")).into()
            }
        })
    }
);

fndr_native_func!(
    /// Overwrites file `file_name` with `data`
    write_func,
    |_, data, file_name| {
        let String(file_name) = &*file_name else {
        return Ok(FenderValue::make_error("file name must be of type `String`").into());
    };
        Ok(match std::fs::write(file_name, data.to_string()) {
            Ok(s) => Null.into(),
            Err(e) => {
                FenderValue::make_error(format!("failed to read file due to error: {e}")).into()
            }
        })
    }
);