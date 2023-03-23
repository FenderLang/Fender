use fender::interpreter::create_vm;
use std::env::args;

fn main() {
    let args = args().skip(1).collect::<Vec<_>>();

    for arg in args {
        match get_fender_code(&arg) {
            Ok(source) => {
                create_vm(source).unwrap().run().unwrap();
            }
            Err(e) => eprintln!("{e}"),
        }
    }
}

fn get_fender_code(name: &str) -> Result<&'static str, String> {
    match name {
        "test" => Ok(include_str!("test.fndr")),
        "fib" => Ok(include_str!("fib.fndr")),
        "blackjack" | "simple_blackjack" => Ok(include_str!("simple_blackjack.fndr")),
        "sort" | "quicksort" => Ok(include_str!("quicksort.fndr")),
        n => Err(format!("can not find program {n:?}")),
    }
}
