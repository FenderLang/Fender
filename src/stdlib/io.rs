use crate::{
    fender_value::FenderValue::{self, *},
    fndr_native_func,
};
use std::{io::Write, ops::Deref};

// --- stdout ---

fndr_native_func!(
    /// Prints to stdout
    print_func,
    |_, item, argv| {
        print!("{}", item.to_string());
        let argv = match &*argv {
            List(l) => l.deref(),
            _ => unreachable!(),
        };
        for item in argv {
            print!("{}", item.to_string());
        }
        Ok(item)
    }
);

fndr_native_func!(
    /// Prints to stdout and adds a newline
    println_func,
    |_, item, argv| {
        print!("{}", item.to_string());
        let argv = match &*argv {
            List(l) => l.deref(),

            e => unreachable!("{:?}", e),
        };
        for item in argv {
            print!("{}", item.to_string());
        }
        println!();

        Ok(item)
    }
);

// --- stdin ---

fndr_native_func!(
    /// Read a single line for stdin
    read_line_func,
    |_| {
        match std::io::stdin().lines().next() {
            Some(Ok(s)) => Ok(String(s.into()).into()),
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
        Ok(match std::fs::read_to_string(file_name.deref()) {
            Ok(s) => String(s.into()).into(),
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
        Ok(match std::fs::write(file_name.deref(), data.to_string()) {
            Ok(s) => Bool(true).into(),
            Err(e) => {
                FenderValue::make_error(format!("failed to write file due to error: {e}")).into()
            }
        })
    }
);

fndr_native_func!(
    /// Appends to file `file_name` with `data`
    append_func,
    |_, data, file_name| {
        let String(file_name) = &*file_name else {
        return Ok(FenderValue::make_error("file name must be of type `String`").into());
    };
        let mut file = match std::fs::OpenOptions::new()
            .write(true)
            .append(true)
            .create(true)
            .open(file_name.deref())
        {
            Ok(f) => f,
            Err(e) => {
                return Ok(FenderValue::make_error(format!(
                    "failed to open file due to error: {e}"
                ))
                .into())
            }
        };

        Ok(match write!(file, "{}", data.to_string()) {
            Ok(s) => Bool(true).into(),
            Err(e) => {
                FenderValue::make_error(format!("failed to append to file due to error: {e}"))
                    .into()
            }
        })
    }
);
