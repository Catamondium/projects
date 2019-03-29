use std::{fmt, ops, env, str, num};// module local, not polluting!

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

        let hrs = nums[0].parse::<u32>()?;
        let mins = nums[1].parse::<u32>()?;

        Ok(Time { hrs, mins})
    }
}

impl fmt::Display for Time {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:02}:{:02}", self.hrs, self.mins)
    }
}

// impl for reference to circumvent move semantics
impl ops::Add<u32> for &Time {
    type Output = Time;
    fn add(self, _rhs: u32) -> Self::Output {
        let t = self.abs() + _rhs;
        Time {
            hrs: t / 60,
            mins: t % 60
        }
    }
}

fn main() {
    let argv: Vec<String> = env::args().collect();
    if argv.len() < 3 {
        panic!("Insufficient arguments")
    }
    let start = argv[1].parse::<Time>();
    let start = match start {
        Ok(t) => t,
        Err(_error) => {
            panic!("_error occured");
        }
    };

    let elapse = if argv[2].contains(':') {
        let this = argv[2].parse::<Time>();
        let this = match this {
            Ok(t) => t,
            Err(_error) => {
                panic!("_error occured");
            }
        };
        this.abs()
    } else {
        let this = argv[2].parse::<u32>();
        let this = match this {
            Ok(t) => t,
            Err(_error) => {
                panic!("_error occured");
            }
        };
        this
    };

    println!("Start:\t{}\t{:+}\nEnd:\t{}",
        start, elapse, &start + elapse)
}
