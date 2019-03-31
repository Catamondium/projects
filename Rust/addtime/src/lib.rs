use std::fmt;
use std::ops;
use std::str;
use std::num;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn abs_mins() {
        let time = Time {hrs: 1, mins: 30};
        assert_eq!(90, time.abs());
    }

    #[test]
    fn addition() {
        let time = Time {hrs: 0, mins: 30};
        let other = 30;

        let result = time + other;
        let abs_result = (result.hrs * 60) + result.mins;

        assert_eq!(60, abs_result);
    }

    #[test]
    fn parse() {
        let subject = "1:30";
        let time: Time = subject.parse().unwrap();

        assert_eq!(time.hrs, 1);
        assert_eq!(time.mins, 30);
    }

    #[test]
    fn display() {
        let time = Time {hrs: 1, mins: 30};
        let result = format!("{}", time);

        assert_eq!("01:30", result);
    }
}

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
