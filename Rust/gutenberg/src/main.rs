use std::char;
use std::collections::HashMap;
use std::env::args;
use std::error::Error;
use std::fs::File;
use std::hash::Hash;
use std::io::{BufRead, BufReader};

#[derive(Debug)]
struct Counthash<K: Hash + Eq>(HashMap<K, usize>);
impl<K: Hash + Eq> Counthash<K> {
    fn new() -> Self {
        Counthash(HashMap::new())
    }

    fn add(&mut self, key: K) {
        let item = self.0.get_mut(&key);
        if let Some(count) = item {
            *count += 1;
        } else {
            self.0.insert(key, 1);
        }
    }

    fn sum(&self) -> usize {
        self.0.iter().map(|(_, v)| v).sum()
    }
}

#[derive(Debug)]
struct Stats {
    counts: Counthash<String>,
    longest: Option<String>,
    shortest: Option<String>,
}

impl Stats {
    fn new() -> Self {
        Stats {
            counts: Counthash::new(),
            longest: None,
            shortest: None,
        }
    }
}

fn analyze(source: BufReader<File>) -> Stats {
    let mut words: Counthash<String> = Counthash::new();
    let mut longest: Option<String> = None;
    let mut shortest: Option<String> = None;

    for line in source.lines().skip(19) {
        let uline = line.unwrap();

        if uline.contains("*** END OF THIS PROJECT GUTENBERG EBOOK") {
            break;
        }

        for word in uline.split(char::is_whitespace).filter(|w| *w != "") {
            words.add(word.clone().to_owned());
            longest = longest
                .filter(|v| v.len() > word.len())
                .or(Some(word.to_string()));

            shortest = shortest
                .filter(|v| v.len() < word.len())
                .or(Some(word.to_string()));
        }
    }

    Stats {
        counts: words,
        longest,
        shortest,
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let files: Vec<String> = args().skip(1).collect();
    let handler = File::open(files[0].clone())?;
    let reader = BufReader::new(handler);

    let stats = analyze(reader);
    let mode = stats.counts.0.iter().fold(None, |acc, (w, c)| {
        acc.filter(|(_, ac)| *ac >= c).or(Some((w, c)))
    });

    println!(
        "count: {}\nmode: {:?}\nlongest: {:?}\nshortest: {:?}",
        stats.counts.sum(),
        mode,
        stats.longest,
        stats.shortest
    );

    Ok(())
}
