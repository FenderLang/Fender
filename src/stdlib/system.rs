use crate::{
    fender_value::FenderValue::{self, *},
    fndr_native_func,
};
use std::{ops::Deref, process::Command};

fndr_native_func!(
    /// Get the current working directory
    ///
    /// This is equivalent to `shell("pwd")` on *nix systems
    pwd_func,
    |_| {
        Ok(match std::env::current_dir() {
            Ok(path) => FenderValue::make_string(path.to_string_lossy().into()).into(),
            Err(e) => FenderValue::make_error(format!("failed to get current path: {}", e)).into(),
        })
    }
);

fndr_native_func!(
    /// Change the current working directory environment variable (PWD)
    cd_func,
    |_, path| {
        let path = match path.unwrap_value() {
            String(s) => s.to_string(),
            e => {
                return Ok(FenderValue::make_error(format!(
                    "invalid arg for `cd`: expected `String` found `{}`",
                    e.get_type_id().to_string()
                ))
                .into())
            }
        };

        Ok(match std::env::set_current_dir(path) {
            Ok(path) => Bool(true).into(),
            Err(e) => {
                FenderValue::make_error(format!("failed to change current directory: {}", e)).into()
            }
        })
    }
);

fndr_native_func!(
    /// Interact with host system shell
    ///
    /// Commands run through `shell` will return all of stdout in a `FenderValue::String` on
    /// success, on failure all of stderr will be returned in a `FenderValue::Error`
    ///
    /// `shell` can operate with 1 to 3 arguments
    ///
    /// One argument will be equivalent to just passing that text into your shell
    /// ```fender
    /// shell("echo hello world > test.txt")
    /// ```
    ///
    /// Two arguments allow you to take in a specific command after receiving arguments that will be passed to the command.
    /// This can be useful to use a shell command in the middle of a function chain
    /// ```fender
    /// $wcResults = readLine().shell("wc").else({println(cannot get file); "0"})
    /// ```
    ///
    /// The default shell command for *nix is `sh -c` and for windows is `batch -c`, if you wish to use a different shell that can
    /// be passed as a third argument
    ///
    /// ```fender
    /// "hello".shell("echo -n", "fish -c")
    /// ```
    shell_func,
    |_, cmd, cmd_name, shell_path| {
        #[cfg(target_os = "windows")]
        const DEFAULT_SHELL: &str = "cmd /c";
        #[cfg(not(target_os = "windows"))]
        const DEFAULT_SHELL: &str = "sh -c";

        let (mut shell, cmd) = match (&*cmd_name, &*shell_path) {
            (String(name), String(shell)) => (
                shell.split(' '),
                format!("{} {}", name.deref(), cmd.to_string()),
            ),
            (String(name), Null) => (
                DEFAULT_SHELL.split(' '),
                format!("{} {}", name.deref(), cmd.to_string()),
            ),
            (Null, Null) => (DEFAULT_SHELL.split(' '), cmd.to_string()),
            _ => {
                return Ok(FenderValue::make_error(format!("unexpected arg types for shell command, expected `(Any, String?, String?)` found `({:?}, {:?}, {:?})", cmd.get_type_id(), cmd_name.get_type_id(), shell_path.get_type_id())).into());
            }
        };

        let shell_name = shell.next().unwrap_or_default();

        let result = match Command::new(shell_name).args(shell).arg(&cmd).output() {
            Ok(v) => v,
            Err(e) => {
                return Ok(FenderValue::make_error(format!(
                    "failed to run command {:?} {:?}\t{}",
                    cmd_name,
                    cmd,
                    e.to_string().trim()
                ))
                .into())
            }
        };

        let output = {
            let output = if result.status.success() {
                result.stdout
            } else {
                result.stderr
            };

            match std::string::String::from_utf8(output) {
                Ok(v) => v,
                Err(_) => "Failed to read command output".into(),
            }
        };

        Ok(if result.status.success() {
            String(output.into())
        } else {
            Error(output)
        }
        .into())
    }
);

// this will be similar to `shell_func` but will have actual details about
// what happened organized in a struct instead of reducing it down to an error or string
//
// fndr_native_func!(process_func, |_, cmd, cmd_name|{
//
// });
