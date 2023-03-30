use fender::interpreter;
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
    } else {
        if let Ok(true) = Path::new(&args[1]).try_exists() {
            fs::read_to_string(&args[1]).unwrap()
        } else {
            args[1].clone()
        }
    };

    let mut vm = match interpreter::create_vm(&script) {
        Ok(v) => v,
        Err(e) => {
            eprintln!("Error processing fender code: {}", e);
            exit(1);
        }
    };

    if let Err(e) = vm.run() {
        println!("{}", e);
    }
}
