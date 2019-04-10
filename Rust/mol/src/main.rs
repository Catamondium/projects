use std::env;
use mol::{mass, normalize};

fn main() {
    let args: Vec<String> = env::args().skip(1).collect();
    for dirty in args {
        let clean = normalize(dirty);
        println!("{}: {} g/mol", clean, mass(&clean));
    }
}
