use std::io::{BufRead, Read, self, BufReader};
use std::rc::Rc;

enum Reciever {
    Command,
    Reading,
}

struct Mediator<T>
where T: Iterator<Item=String>
{
    source: T,
    current: Option<String>,
}

impl<T: Iterator<Item = String>> Mediator<T> {
    fn new<F: BufRead>(stream: F) -> Self {
        let mut iter = stream.lines().filter_map(|x| x.ok()).filter(|l| l != "");
        let current = iter.next();
        Mediator { source: iter, current }
    }
}