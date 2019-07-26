use std::char;
use std::collections::HashMap;
use std::env::args;
use std::error::Error;
use std::fs::File;
use std::hash::Hash;
use std::io::{BufRead, BufReader};


/* TODO
 * multithread across multiple files
 * * may require contrival for practice
 * * time analysis?
 */

#[derive(Debug)]
struct Counthash<K: Hash + Eq + Clone> {
    data: HashMap<K, usize>,
}

impl<K: Hash + Eq + Clone> Counthash<K> {
    fn new() -> Self {
        Counthash {
            data: HashMap::new(),
        }
    }

    fn add(&mut self, key: &K) {
        let item = self.data.get_mut(&key);
        if let Some(count) = item {
            *count += 1;
        } else {
            self.data.insert(key.clone(), 1);
        }
    }

    fn sum(&self) -> usize {
        self.data.values().sum()
    }
}

#[derive(Debug)]
struct Stats {
    title: String,
    count: usize,
    mode: Option<(String, usize)>,
    longest: Option<String>,
    shortest: Option<String>,
}

impl Stats {
    fn new() -> Self {
        Stats {
            title: "".to_owned(),
            count: 0,
            mode: None,
            longest: None,
            shortest: None,
        }
    }
}

fn unmut<A: Clone, B: Clone>(opt: &Option<(&A, &B)>) -> Option<(A, B)> {
    opt.map(|(a, b)| (a.clone(), b.clone()))
}

fn analyze(source: BufReader<File>) -> Stats {
    let mut words: Counthash<String> = Counthash::new();

    let mut lines = source
        .lines()
        .filter_map(|x| x.ok())
        .skip_while(|x| !x.contains("Title: "));
    let title_line = lines.next().expect("No title");
    let title: String = title_line[7..].to_string();

    for line in lines
        .skip_while(|x| !x.contains("*** START OF THIS PROJECT GUTENBERG EBOOK"))
        .skip(1)
    {
        if line.contains("*** END OF THIS PROJECT GUTENBERG EBOOK") {
            break;
        }
        for word in line.split(char::is_whitespace).filter(|w| *w != "") {
            words.add(&word.to_string());
        }
    }

    let mode = words.data.iter().fold(None, |acc, (w, c)| {
        acc.filter(|(_, ac)| *ac > c).or(Some((w, c)))
    });

    let shortest = words.data.keys().fold(None, |acc: Option<String>, w| {
        acc.filter(|aw| aw.len() < w.len()).or(Some(w.to_owned()))
    });

    let longest = words.data.keys().fold(None, |acc: Option<String>, w| {
        acc.filter(|aw| aw.len() > w.len()).or(Some(w.to_owned()))
    });

    Stats {
        title,
        count: words.sum(),
        mode: unmut(&mode),
        longest,
        shortest,
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    //let files: Vec<String> = args().skip(1).collect();
    for (i, file) in args().skip(1).enumerate() {
        let handler = File::open(file.clone())?;
        let reader = BufReader::new(handler);

        let stats = analyze(reader);

        println!(
            "title: {}\ncount: {}\nmode: {:?}\nlongest: {:?}\nshortest: {:?}",
            stats.title, stats.count, stats.mode, stats.longest, stats.shortest
        );

        if i + 1 != args().skip(1).count() {
            print!("\n");
        }
    }
    Ok(())
}
