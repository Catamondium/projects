extern crate rand;
extern crate reqwest;

use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::sync::{mpsc, Arc, Mutex};
use std::thread;

const POPULATION: usize = 100_000;
const SAMPLERATIO: f64 = 0.0001;
const SAMPLE: usize = ((POPULATION as f64) * SAMPLERATIO) as usize;

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
enum Msg {
    Active,
    Inactive,
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
/// to allow proper mutex utilization.
struct Producer<T>
where
    T: Iterator<Item = usize>,
{
    pub source: T,
}

impl<T> Producer<T>
where
    T: Iterator<Item = usize>,
{
    pub fn new(source: T) -> Producer<T> {
        Producer { source }
    }
}

impl<T> Iterator for Producer<T>
where
    T: Iterator<Item = usize>,
{
    type Item = String;
    fn next(&mut self) -> Option<Self::Item> {
        self.source.next().and_then(|id| {
            let site = format!(
                "https://www.example.co.uk/product_info.php?products_id={:05}",
                id
            );
            reqwest::get(site.as_str())
                .ok()
                .and_then(|mut x| x.text().ok())
        })
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

fn crawl<T>(chan: mpsc::Sender<Msg>, mutex: &mut Arc<Mutex<Producer<T>>>)
where
    T: Iterator<Item = usize>,
{
    let mut dat: Option<String>;

    {
        let mut itr = mutex.lock().unwrap();
        dat = itr.next();
    }

    match dat {
        Some(body) => {
            if body.contains("Product not found!") {
                chan.send(Msg::Inactive).unwrap();
            } else {
                chan.send(Msg::Active).unwrap();
            }
        }

        None => {
            chan.send(Msg::Err).unwrap();
        }
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let set = random_set(SAMPLE, POPULATION);

    let mtx = Arc::new(Mutex::new(Producer::new(set.into_iter())));
    let (tx, rx): (mpsc::Sender<Msg>, mpsc::Receiver<Msg>) = mpsc::channel();
    for _ in 0..SAMPLE {
        let ctx = tx.clone();
        let mut cmtx = mtx.clone();
        thread::spawn(move || {
            crawl(ctx, &mut cmtx);
        });
    }

    let mut counts: HashMap<Msg, usize> = map! {
        Msg::Active => 0,
        Msg::Inactive => 0,
        Msg::Err => 0
    };

    for _ in 0..SAMPLE {
        let msg = rx.recv().unwrap();
        let cnt = counts.get_mut(&msg).unwrap();
        *cnt += 1;
    }

    println!("Active: {}", counts.get(&Msg::Active).unwrap());
    println!("Inactive: {}", counts.get(&Msg::Inactive).unwrap());
    println!("Errors: {}", counts.get(&Msg::Err).unwrap());

    println!(
        "~{:02}% activity",
        percent(&counts.get(&Msg::Active).unwrap(), &SAMPLE)
    );
    println!(
        "~{} actually active",
        scale(&counts.get(&Msg::Active).unwrap(), &SAMPLERATIO)
    );

    return Ok(());
}
