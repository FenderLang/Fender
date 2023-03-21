use fender::interpreter;
use std::{fs, path::Path, process::exit};

fn main() {
    let file = std::env::args().nth(1).unwrap();
    let file = Path::new(&file);
    let script = fs::read_to_string(file).unwrap();
    let mut vm = match interpreter::create_vm(&script){
        Ok(v) => v,
        Err(e) => {eprintln!("Error proccessing fender code: {}", e); exit(1);},
    };
    vm.run().unwrap();
}
