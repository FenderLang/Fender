use fender::interpreter;
use std::{fs, path::Path};

fn main() {
    let file = std::env::args().nth(1).unwrap();
    let file = Path::new(&file);
    let script = fs::read_to_string(file).unwrap();
    let mut vm = interpreter::create_vm(&script).unwrap();
    vm.run().unwrap();
}
