use std::fs;
use std::path::Path;
use fender::interpreter;

fn main() {
    let file = std::env::args().nth(1).unwrap();
    let file = Path::new(&file);
    let script = fs::read_to_string(file).unwrap();
    let mut vm = interpreter::create_vm(&script).unwrap();
    vm.run().unwrap();
}
