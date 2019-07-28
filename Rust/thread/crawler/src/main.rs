extern crate rand;
extern crate reqwest;

use std::collections::{HashMap, HashSet};
use std::sync::{mpsc, Arc, Mutex};
use std::thread;

const POPULATION: usize = 100_000;
const SAMPLERATIO: f64 = 0.001;
const SAMPLE: usize = ((POPULATION as f64) * SAMPLERATIO) as usize;

#[derive(PartialEq, Eq, Debug, Hash)]
enum Msg {
    Activity(bool),
    Err,
}

macro_rules! map(
    { $($key:expr => $value:expr),+ } => {
        {
            let mut m = ::std::collections::HashMap::new();
            $(
                m.insert($key, $value);
            )+
            m
        }
     };
);

/// templated http request iterator,
/// adapts reqwest::get calls to struct
/// to mutex network calls.
struct Producer<T: Iterator<Item = usize>>(T);

impl<T: Iterator<Item = usize>> Producer<T> {
    pub fn new(source: T) -> Producer<T> {
        Producer(source)
    }
}

impl<T: Iterator<Item = usize>> Iterator for Producer<T> {
    type Item = String;
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().and_then(|id| {
            let site = format!(
                "https://www.example.co.uk/product_info.php?products_id={:05}",
                id
            );
            reqwest::get(site.as_str())
                .ok()
                .and_then(|mut x| x.text().ok())
        })
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

/// Generate random set in range [0, max), of length n.
/// panics if max < n.
fn random_set(n: usize, max: usize) -> HashSet<usize> {
    if max < n {
        panic!(format!(
            "Insufficient range: [0, {}) to fill {} spots",
            max, n
        ))
    }

    let mut set = HashSet::new();

    while set.len() < n {
        // Will bias distribution
        // next best implementation to avoid faff
        let big: usize = rand::random();
        set.insert(big % max);
    }

    return set;
}

fn percent(x: &usize, max: &usize) -> usize {
    return (100 * x) / max;
}

fn scale(x: &usize, ratio: &f64) -> usize {
    return ((*x as f64) / ratio) as usize;
}

fn crawl<T>(chan: mpsc::SyncSender<Msg>, mutex: &mut Arc<Mutex<Producer<T>>>)
where
    T: Iterator<Item = usize>,
{
    let mut dat: Option<String>;

    {
        let mut itr = mutex.lock().unwrap();
        dat = itr.next();
    }

    let msg = if let Some(body) = dat {
        Msg::Activity(body.contains("Product not found!"))
    } else {
        Msg::Err
    };
    chan.send(msg).unwrap();
}

fn main() {
    let set = random_set(SAMPLE, POPULATION);

    let mtx = Arc::new(Mutex::new(Producer::new(set.into_iter())));
    let (tx, rx) = mpsc::sync_channel::<Msg>(SAMPLE);
    for _ in 0..SAMPLE {
        let ctx = tx.clone();
        let mut cmtx = mtx.clone();
        thread::spawn(move || {
            crawl(ctx, &mut cmtx);
        });
    }
    drop(tx); // allows other threads to terminate for hangup

    let mut counts: HashMap<Msg, usize> = map! {
        Msg::Activity(true) => 0,
        Msg::Activity(false) => 0,
        Msg::Err => 0
    };

    for msg in rx {
        let cnt = counts.get_mut(&msg).unwrap();
        *cnt += 1;
    }
    println!(
        "Active:\t{} / {}",
        counts.get(&Msg::Activity(true)).unwrap(),
        SAMPLE
    );
    println!("Errors:\t{}", counts.get(&Msg::Err).unwrap());

    println!(
        "~{:02}% activity",
        percent(&counts.get(&Msg::Activity(true)).unwrap(), &SAMPLE)
    );
    println!(
        "~{} actually active",
        scale(&counts.get(&Msg::Activity(true)).unwrap(), &SAMPLERATIO)
    );
}
