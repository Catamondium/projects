extern crate rand;
extern crate reqwest;

use std::collections::HashSet;
use std::error::Error;
use std::vec::Vec;

const POPULATION: usize = 100_000;
const SAMPLERATIO: f64 = 0.1;
const SAMPLE: usize = ((POPULATION as f64) * SAMPLERATIO) as usize;

// TODO
// // multithreading w/ http mutexed

#[derive(PartialEq, Debug, Clone)]
enum Msg {
    Active,
    Inactive,
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

fn percent(x: usize, max: usize) -> usize {
    return (100 * x) / max;
}

fn scale(x: usize, ratio: f64) -> usize {
    return ((x as f64) / ratio) as usize;
}

fn crawl(id: usize) -> Result<Msg, Box<dyn Error>> {
    let source = format!(
        "https://www.example.co.uk/product_info.php?products_id={:05}",
        id
    );

    let resp = reqwest::get(source.as_str())?.text()?;
    if resp.contains("Product not found!") {
        return Ok(Msg::Inactive);
    } else {
        return Ok(Msg::Active);
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let set = random_set(10, 100);
    let mut results = Vec::new();
    println!("{:#?}", set);
    for i in set {
        results.push(crawl(i));
    }
    let filtered: Vec<Msg> = results.into_iter().filter_map(|e| e.ok()).collect();
    println!(
        "Active: {}\nInactive: {}",
        filtered.iter().filter(|x| **x == Msg::Active).count(),
        filtered.iter().filter(|x| **x == Msg::Inactive).count()
    );

    return Ok(());
}
