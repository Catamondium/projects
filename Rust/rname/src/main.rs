extern crate regex;
use regex::{escape, Regex};

use std::path::*;
use std::{env, fs};

fn re_sort<'a>(dir: &'a Vec<PathBuf>, parent: &str, width: &f64) -> Vec<&'a PathBuf> {
    let re_string = format!(r"^{}-\d{{{}}}.*$", escape(&parent), width);
    let re = Regex::new(&re_string).unwrap();

    let (mut matched, mut unmatched): (Vec<&PathBuf>, Vec<&PathBuf>) = dir.iter().partition(|e| {
        if let Some(x) = e.file_name() {
            if let Some(y) = x.to_str() {
                return re.is_match(y);
            } else {
                return false;
            }
        } else {
            return false;
        }
    });

    println!("Re: {:?}", re);
    println!("Matched: {:?}\nUnmatched: {:?}", matched, unmatched);

    matched.sort();
    unmatched.append(&mut matched);

    unmatched
}

fn is_directory(thing: &fs::DirEntry) -> bool {
    if let Ok(typename) = thing.file_type() {
        return typename.is_dir();
    }
    false
}

fn rename(reldir: &Path) {
    if !reldir.is_dir() {
        return;
    }

    let dir = reldir.canonicalize().unwrap();

    println!(
        "in Path: {:?}",
        dir.canonicalize().unwrap().file_name().unwrap()
    );

    if let Ok(iter) = fs::read_dir(&dir) {
        let filtered = iter.filter_map(|x| x.ok());
        let (dirs, files): (Vec<fs::DirEntry>, Vec<fs::DirEntry>) =
            filtered.partition(is_directory);

        for _d in dirs {
            //rename(&d.path()); // Don't recurse when testing
        }

        let fpaths: Vec<PathBuf> = files.iter().map(|f| f.path()).collect();
        let width = (fpaths.len() as f64).log10().ceil();
        let dirname = dir.file_name().unwrap().to_str().unwrap();

        let sorted = re_sort(&fpaths, &dirname, &width);

        for (i, f) in sorted.iter().enumerate() {
            let ext = match f.extension() {
                Some(e) => e.to_str().unwrap_or(""),
                None => "",
            };

            let newname = format!("{}-{:01}.{}", dirname, i, ext);
            println!("{:?} -> \"{}\"", f.file_name().unwrap(), newname);
        }
    }
}

fn target(args: &Vec<String>) -> Vec<String> {
    if args.len() == 0 {
        if let Ok(pwd) = env::var("PWD") {
            return vec![pwd];
        } else {
            return Vec::new();
        }
    } else {
        return args.clone();
    }
}

fn main() {
    let mut args: Vec<String> = env::args().skip(1).collect();
    args = target(&args);

    println!("Args: {:?}", args);

    for x in args {
        rename(Path::new(&x));
    }
}
