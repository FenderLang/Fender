use fender::interpreter;
use std::env::args;

fn main() {
    let args = args().skip(1).collect::<Vec<_>>();

    for arg in args {
        match get_fender_code(&arg) {
            Ok(source) => {
                let (mut engine, main_func) = match interpreter::create_engine_main(source) {
                    Ok(v) => v,
                    Err(e) => {
                        eprintln!("Error proccessing fender code: {}", e);
                        continue;
                    }
                };
                match engine.call(&main_func, Vec::new()) {
                    Ok(v) => println!("\n\n\nEXECUTION OUTPUT:\n=====\n{:?}\n=====", v),
                    Err(e) => eprintln!("Fender execution error: {}", e),
                }
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
