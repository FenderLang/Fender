#![feature(generic_arg_infer)]
#[cfg(feature = "repl")]
use fender::interpreter::repl::FenderRepl;
use fender::{
    interpreter,
    type_sys::{fender_reference::FenderReference, fender_value::FenderValue},
};
use std::{fs, path::Path, process::exit};

fn main() {
    let args = std::env::args().collect::<Vec<_>>();
    let script = if args.len() < 2 {
        #[cfg(feature = "repl")]
        {
            FenderRepl::new().run();
            return;
        }
        #[cfg(not(feature = "repl"))]
        {
            let mut script = String::new();
            let stdin = std::io::stdin();

            for line in stdin.lines() {
                match line {
                    Ok(l) => script.push_str(&l),
                    Err(_) => break,
                };
                script.push('\n');
            }
            script
        }
    } else if let Ok(true) = Path::new(&args[1]).try_exists() {
        fs::read_to_string(&args[1]).unwrap()
    } else {
        args[1].clone()
    };

    let (mut engine, main) = match interpreter::create_engine_main(&script) {
        Ok(v) => v,
        Err(e) => {
            eprintln!(
                "Error processing fender code: {}",
                e.src_relative_string(&script)
            );
            exit(1);
        }
    };

    match engine.call(
        &main,
        if args.len() > 2 {
            args[2..]
                .iter()
                .map(|v| FenderReference::from(FenderValue::make_string(v.clone())))
                .collect()
        } else {
            Vec::new()
        },
    ) {
        Ok(v) => match v.unwrap_value() {
            FenderValue::Int(i) => exit(i as i32),
            FenderValue::Bool(b) => exit((!b) as i32),
            FenderValue::Null => (),
            _ => (), // do we want to print this? or do any of this?
        },
        Err(e) => {
            eprintln!("{}", e);
        }
    }
}
