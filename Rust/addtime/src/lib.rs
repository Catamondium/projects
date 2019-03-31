use std::{fmt, ops, str, num };

#[derive(Clone, Copy)]
pub struct Time {
    pub hrs: u32,
    pub mins: u32
}

impl Time {
    pub fn abs(&self) -> u32 {
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
