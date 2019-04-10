extern crate regex;
use regex::Regex;
#[macro_use]
extern crate lazy_static;

use std::collections::HashMap;
use std::error::Error;

const PTABLE_FILE : &str = include_str!("ptable.tsv");

type TransTable = HashMap<char, char>;
type Ptable = HashMap<String, f32>;

lazy_static! {
    static ref PTABLE:  Ptable = load_table();
    static ref TTABLE:  TransTable = trans("[]{}", "()");

    static ref COEFFRE: Regex = Regex::new(r"^(\d+)").unwrap();  // beginning coefficient
    static ref TOKRE:   Regex = Regex::new(r"\(.*?\)|([A-Z][a-z]*)(\d*)").unwrap();  // groups: Elem, [coeff]
    static ref SUBRE:   Regex = Regex::new(r"\((.*)\)(\d*)").unwrap(); // groups: expr, [coeff]
}

fn load_table() -> Ptable {
    let mut out: Ptable = HashMap::new();

    for line in PTABLE_FILE.lines() {
        let fields: Vec<&str> = line.split("\t").collect();
        if out.contains_key(&fields[0].to_string()) {
            panic!(format!("Repeated element: {}", fields[0]));
        };

        out.insert(
            fields[0].to_string(),
            fields[1].parse::<f32>()
                .expect("Source table error")
        );
    }
    out
}

fn trans(from: &str, to: &str) -> TransTable {
    from.chars()
        .zip(to.chars().cycle())
        .collect()
}

fn normalize(dirty: String) -> String {
    let mut clean = String::new();

    for c in dirty.chars() {
        match TTABLE.get(&c) {
            Some(rep) => clean.push(*rep),
            None => clean.push(c)
        }
    }

    clean
        .chars()
        .filter(|c| {
            *c == '(' || *c == ')' || c.is_alphanumeric()
        }).collect()
}

fn mass(comp: &String) -> f32 {
    let bigcoeff = match COEFFRE.captures(&comp) {
        Some(x) => x[1].parse::<f32>().unwrap_or(1.0),
        None    => 1.0
    };
    
    let mut acc = 0.0;

    for cap in TOKRE.captures_iter(&comp) {
        let mut temp = 0.0;
        match cap.get(1) {
            Some(elem)  => {
                temp = *PTABLE.get(&elem.as_str().to_string())
                        .unwrap_or(&0.0)
            },
            None        => ()
        }

        match cap.get(2) {
            Some(coeff) => {
                temp *= coeff.as_str().parse().unwrap_or(1.0);
            },
            None        => ()
        }

        acc += temp
    }

    for cap in SUBRE.captures_iter(&comp) {
        let mut temp = 0.0;
        match cap.get(1) {
            Some(subexpr)   => {
                temp += mass(&subexpr.as_str().to_string());
            },
            None            => ()
        }

        match cap.get(2) {
            Some(coeff) => {
                temp *= coeff.as_str().parse().unwrap_or(1.0);
            },
            None        => ()
        }
        acc += temp;
    }

    bigcoeff * acc
}

fn main() {
    let sample = "5CH(CH3)2".to_string(); // 114 g/mol
    println!("{}: {} g/mol", sample, mass(&sample));
}
