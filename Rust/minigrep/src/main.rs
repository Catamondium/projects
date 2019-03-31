extern crate getopts;
use getopts::Options;

use std::{env, process};

use minigrep;
use minigrep::Config;

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut opts = Options::new();
    opts.optflag("i", "ignorecase", "Case insensitivity");
    let matches = opts.parse(&args[1..]).unwrap_or_else(|err| {
        eprintln!("Parsing error: {}", err);
        process::exit(1);
    });

    let config = Config::new(&matches).unwrap_or_else(|err| {
        eprintln!("Parsing error: {}", err);
        process::exit(1);
    });

    if let Err(e) = minigrep::run(config) {
        eprintln!("Runtime error: {}", e);
        process::exit(1);
    }
}