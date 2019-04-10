extern crate regex;
use regex::Regex;
#[macro_use]
extern crate lazy_static;

use std::collections::HashMap;

type TransTable = HashMap<char, char>;
type Ptable = HashMap<String, f32>;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[should_panic]
    fn none_element() {
        mass(&"5Haag5".to_string());
    }

    #[test]
    fn normalization() {
        let dirty: String = "C   5#[20]{30}".to_string();
        assert_eq!(
            "C5(20)(30)".to_string(),
            normalize(dirty)
            );
    }

    #[test]
    fn element_carbon() {
        assert_eq!(
            12.01,
            mass(&"C".to_string())
            );
    }

    #[test]
    fn simple_ethane() { 
        assert_eq!(
            30.02,
            mass(&"C2H6".to_string())
            ); 
    }

    #[test]
    fn five_struct_dimethylpropane() {
        assert_eq!(
            5.0 * 72.05, 
            mass(&"5C(CH3)4".to_string())
            );
    }
}

const PTABLE_FILE : &str = include_str!("ptable.tsv");
lazy_static! {
    static ref PTABLE:  Ptable = load_table();
    static ref TTABLE:  TransTable = trans("[]{}", "()");

    static ref COEFFRE: Regex = Regex::new(r"^(\d+)").unwrap();  // beginning coefficient
    static ref TOKRE:   Regex = Regex::new(r"\(.*?\)|([A-Z][a-z]*)(\d*)").unwrap();  // groups: Elem, [coeff]
    static ref SUBRE:   Regex = Regex::new(r"\((.*)\)(\d*)").unwrap(); // groups: expr, [coeff]
}

fn make_coeff(src: std::option::Option<regex::Match<'_>>) -> f32 {
    if let Some(coeff) = src {
        return coeff.as_str().parse::<f32>().unwrap_or(1.0);
    } else {
        1.0
    }
}

fn load_table() -> Ptable {
    let mut out: Ptable = HashMap::new();

    for line in PTABLE_FILE.lines() {
        let fields: Vec<&str> = line.split("\t").collect();
        if out.contains_key(fields[0]) {
            panic!(format!("Table error: Repeated element: {}", fields[0]));
        };

        out.insert(
            fields[0].to_string(),
            fields[1].parse::<f32>()
                .expect(&format!("Table error: bad float at {}", fields[0]))
        );
    }
    out
}

fn trans(from: &str, to: &str) -> TransTable {
    from.chars()
        .zip(to.chars().cycle())
        .collect()
}

pub fn normalize(dirty: String) -> String {
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

pub fn mass(comp: &String) -> f32 {
    let bigcoeff = match COEFFRE.captures(&comp) {
        Some(x) => x[1].parse::<f32>().unwrap_or(1.0),
        None    => 1.0
    };
    
    let mut acc = 0.0;

    for cap in TOKRE.captures_iter(&comp) {
        let mut temp = 0.0;
        if let Some(elem) = cap.get(1) {
            let key = elem.as_str();
            if let Some(efloat) = PTABLE.get(key) {
                temp = *efloat;
            } else {
                let err = format!("Element {} doesn't exist", key);
                panic!(err);
            }
        }

        temp *= make_coeff(cap.get(2));
        acc += temp;
    }

    for cap in SUBRE.captures_iter(&comp) {
        let mut temp = 0.0;
        if let Some(subexpr) = cap.get(1) {
            temp = mass(&subexpr.as_str().to_string());
        }
        
        temp *= make_coeff(cap.get(2));
        acc += temp;
    }

    bigcoeff * acc
}