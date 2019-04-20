extern crate getopts;
use getopts::Options;

use std::env;
use std::error::Error;
use std::io::prelude::*;
use std::path::Path;

use rname::*;

fn verify(path: &str) -> bool {
    println!("About to rename inside:\t{}\nAre you sure?", path);
    let mut buf = [0; 1];
    let err = std::io::stdin().read(&mut buf).is_ok();
    let c = std::str::from_utf8(&buf).unwrap_or("n");
    err && c.to_lowercase() == "y"
}

fn usage(program: &str, opts: &Options) {
    let brief = format!("Usage: {} [-dvrfih] [DIR]...\nBatch renamer following DIR-#.ext pattern.\n# is an integer zfilled to width log10 of the number of files to mv recursing directories", program);
    print!("{}", opts.usage(&brief));
}

fn parse(args: &Vec<String>) -> (Vec<String>, Config, bool) {
    let program = args[0].clone();

    let mut opts = Options::new();
    opts.optflag("d", "dry_run", "Run without renaming anything, verbosely");
    opts.optflag("v", "verbose", "Show all possible moves");
    opts.optflag("q", "quiet", "Show no moves");
    opts.optflag("r", "recurse", "Recurse through subdirectories");
    opts.optflag("f", "force", "Don't ask for approval");
    opts.optflag("i", "interactive", "Require approval");
    opts.optflag("h", "help", "Show this usage");
    let matches = opts.parse(&args[1..]).unwrap_or_else(|e| {
        eprintln!("Bad option: {}", e);
        usage(&program, &opts);
        std::process::exit(1);
    });

    if matches.opt_present("h") {
        usage(&program, &opts);
        std::process::exit(0);
    }

    let conf = Config {
        dry: matches.opt_present("d"),
        verbose: matches.opt_present("d")
            || (matches.opt_present("v") && !matches.opt_present("q")),
        recurse: matches.opt_present("r"),
    };

    let force = matches.opt_present("f") && !matches.opt_present("i");

    let mut ret_vec = matches.free;
    if ret_vec.len() == 0 {
        if let Ok(pwd) = env::var("PWD") {
            ret_vec.push(pwd);
        }
    }

    (ret_vec, conf, force)
}

fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let (args, config, force) = parse(&args);

    for x in args {
        if config.dry || force || verify(&x) {
            mv(Path::new(&x), &config)?;
        }
    }
    Ok(())
}
