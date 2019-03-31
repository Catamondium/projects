extern crate getopts;
use getopts::Options;

use std::{env, process};
use std::error::Error;

use addtime;
use addtime::Time;

fn usage(program: &str, opts: &Options) {
    let brief = format!("Usage: {} [-hq] <HH:MM> <mins | HH:MM>", program);
    print!("{}", opts.usage(&brief));
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let program = args[0].clone();

    let mut opts = Options::new();
    opts.optflag("h", "help", "print this usage");
    opts.optflag("q", "quiet", "Only display end time");
    let matches = opts.parse(&args[1..]).unwrap_or_else(|err| {
        eprintln!("Bad option: {}", err);
        usage(&program, &opts);
        process::exit(1);
    });

    if matches.opt_present("h") {
        usage(&program, &opts);
        return;
    }

    let (start, elapse) = parse_args(&matches.free).unwrap_or_else(|err| {
        eprintln!("Parsing error: {}", err);
        usage(&program, &opts);
        process::exit(1);
        });

    if matches.opt_present("q") {
        println!("{}", start + elapse);
    } else {
        println!("Start:\t{}\t{:+}\nEnd:\t{}",
            start, elapse, start + elapse);
    }
}

fn parse_args(args: &Vec<String>) -> Result<(Time, u32), Box<dyn Error>> {
    if args.len() < 2 {
        Err("Insufficient arguments")?;
    }

    let start = args[0].parse::<Time>()?;

    let elapse = if args[1].contains(':') {
        args[1].parse::<Time>()?.abs()
    } else {
        args[1].parse::<u32>()?
    };

    Ok((start, elapse))
}
