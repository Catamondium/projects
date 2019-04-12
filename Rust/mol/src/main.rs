use mol::{mass, normalize};
use std::{env, process};

fn main() {
    let args: Vec<String> = env::args().skip(1).collect();
    for dirty in args {
        let clean = normalize(dirty);
        match mass(&clean) {
            Ok(m)   => {
                println!("{}: {} g/mol", clean, m);
                },
            Err(e)  => {
                eprintln!("{}", e);
                process::exit(1);
            }
        }
    }
}
