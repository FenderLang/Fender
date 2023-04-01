use fender::{fender_reference::FenderReference, fender_value::FenderValue, interpreter};
use std::{fs, io::BufRead, path::Path, process::exit};

fn main() {
    let args = std::env::args().collect::<Vec<_>>();
    let script = if args.len() < 2 {
        let mut script = String::new();
        let stdin = std::io::stdin();
        for line in stdin.lock().lines() {
            match line {
                Ok(l) => script.push_str(&l),
                Err(_) => break,
            };
            script.push('\n');
        }
        script
    } else if let Ok(true) = Path::new(&args[1]).try_exists() {
        fs::read_to_string(&args[1]).unwrap()
    } else {
        args[1].clone()
    };

    let (mut engine, main) = match interpreter::create_engine_main(&script) {
        Ok(v) => v,
        Err(e) => {
            eprintln!("Error processing fender code: {:#?}", e); // remove later, just for debugging rust
            eprintln!(
                "Error processing fender code: {}",
                e.src_relative_string(&script)
            );
            exit(1);
        }
    };

    match engine.call(
        &main,
        args[2..]
            .iter()
            .map(|v| FenderReference::from(FenderValue::make_string(v.clone())))
            .collect(),
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
