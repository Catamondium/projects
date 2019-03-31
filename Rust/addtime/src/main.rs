use std::{env, process};
use std::error::Error;

use addtime;
use addtime::Time;

fn main() {
    let args: Vec<String> = env::args().collect();

    let (start, elapse) = parse_args(&args).unwrap_or_else(|err| {
        println!("Parsing error: {}", err);
        process::exit(1);
        });

    println!("Start:\t{}\t{:+}\nEnd:\t{}",
        start, elapse, start + elapse)
}

fn parse_args(args: &Vec<String>) -> Result<(Time, u32), Box<dyn Error>> {
    if args.len() < 3 {
        Err("Insufficient arguments")?;
    }

    let start = args[1].parse::<Time>()?;

    let elapse = if args[2].contains(':') {
        args[2].parse::<Time>()?.abs()
    } else {
        args[2].parse::<u32>()?
    };

    Ok((start, elapse))
}
