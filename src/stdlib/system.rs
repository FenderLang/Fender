use crate::{
    fender_value::FenderValue::{self, *},
    fndr_native_func,
};
use std::process::Command;

fndr_native_func!(shell_func, |_, cmd, cmd_name, shell_path| {
    #[cfg(target_os = "windows")]
    const DEFAULT_SHELL: &'static str = "batch -c";
    #[cfg(not(target_os = "windows"))]
    const DEFAULT_SHELL: &'static str = "sh -c";

    let (mut shell, cmd) = match (&*cmd_name, &*shell_path) {
        (String(name), String(shell)) => (shell.split(' '), format!("{} {}", name, cmd.to_string())),
        (String(name), Null) => (DEFAULT_SHELL.split(' '), format!("{} {}", name, cmd.to_string())),
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
                "failed to run command {:?} {:?}",
                cmd_name, cmd
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
        String(output)
    } else {
        Error(output)
    }
    .into())
});

// this will be similar to `shell_func` but will have actual details about
// what happened organized in a struct instead of reducing it down to an error or string
//
// fndr_native_func!(process_func, |_, cmd, cmd_name|{
//
// });
