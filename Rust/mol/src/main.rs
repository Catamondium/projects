//extern crate regex;
//use regex::Regex;

use std::collections::HashMap;

const PTABLE_FILE : &str = include_str!("ptable.tsv");

type TransTable = HashMap<char, char>;
type Ptable = HashMap<String, f32>;
/* From Python
COEFFRE = r"^(\d+)"  // beginning coefficient
TOKRE = r"\(.*?\)|([A-Z][a-z]*)(\d*)"  // groups: Elem, [coeff]
SUBRE = r"\((.*)\)(\d*)" // groups: expr, [coeff]
*/
//struct Regexes(Regex, Regex, Regex); // Compress passing

fn load_table() -> Ptable {
    let mut out: Ptable = HashMap::new();

    for line in PTABLE_FILE.lines() {
        let fields: Vec<&str> = line.split("\t").collect();
        out.insert(
            fields[0].to_string(),
            fields[1].parse()
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

fn normalize(dirty: String, table: TransTable) -> String {
    let mut clean = String::new();

    for c in dirty.chars() {
        match table.get(&c) {
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

fn main() {
    println!("{:#?}", load_table());
}
