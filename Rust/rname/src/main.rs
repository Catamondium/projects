extern crate regex;
use regex::{escape, Regex};

use std::path::*;
use std::{env, fs};

fn re_sort<'a>(dir: &'a Vec<PathBuf>, parent: &Path) -> Vec<&'a PathBuf> {
    let width = (dir.len() as f64).log10().ceil();
    let re_string = format!(
        r"^{}-\d{{{}}}.*$",
        escape(&parent.file_name().unwrap().to_str().unwrap()),
        width
    );
    let re = Regex::new(&re_string).unwrap();

    let (mut matched, mut unmatched): (Vec<&PathBuf>, Vec<&PathBuf>) = dir
        .iter()
        .partition(|e| re.is_match(e.file_name().unwrap().to_str().unwrap()));

    println!("Re: {:?}", re);
    println!("Matched: {:?}\nUnmatched: {:?}", matched, unmatched);

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

        for d in dirs {
            //rename(&d.path()); // Don't recurse when testing
        }

        let fpaths: Vec<PathBuf> = files.iter().map(|f| f.path()).collect();
        re_sort(&fpaths, &dir);
        for f in fpaths {}
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
