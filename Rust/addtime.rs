use std::{env, process};
use std::{fmt, ops, str, num };
use std::error::Error;// module local, not polluting!

#[derive(Clone, Copy)]
struct Time {
    hrs: u32,
    mins: u32
}

impl Time {
    fn abs(&self) -> u32 {
        (self.hrs * 60) + self.mins
    }
}

impl str::FromStr for Time {
    type Err = num::ParseIntError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let nums: Vec<&str> = s.trim()
            .split(':')
            .collect();

        let hrs: u32 = nums[0].parse()?;
        let mins: u32 = nums[1].parse()?;

        Ok(Time { hrs, mins})
    }
}

impl fmt::Display for Time {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:02}:{:02}", self.hrs, self.mins)
    }
}

impl ops::Add<u32> for Time {
    type Output = Time;
    fn add(self, rhs: u32) -> Self::Output {
        let t = self.abs() + rhs;
        Time {
            hrs: t / 60,
            mins: t % 60
        }
    }
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

fn main() {
    let args: Vec<String> = env::args().collect();

    let (start, elapse) = parse_args(&args).unwrap_or_else(|err| {
        println!("Parsing error: {}", err);
        process::exit(1);
        });

    println!("Start:\t{}\t{:+}\nEnd:\t{}",
        start, elapse, start + elapse)
}
