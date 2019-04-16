use std::path::*;
use std::{env, fs};

fn is_directory(thing: &fs::DirEntry) -> bool {
    if let Ok(typename) = thing.file_type() {
        return typename.is_dir();
    }
    false
}

fn rename(dir: &Path) {
    if !dir.is_dir() {
        return;
    }

    println!(
        "in Path: {:?}",
        dir.canonicalize().unwrap().file_name().unwrap()
    );

    if let Ok(iter) = fs::read_dir(dir) {
        let filtered = iter.filter_map(|x| x.ok());
        let (dirs, files): (Vec<fs::DirEntry>, Vec<fs::DirEntry>) =
            filtered.partition(is_directory);

        for d in dirs {
            println!("Dir:\t{:?}", d);
            //rename(&d.path()); // Don't recurse when testing
        }

        let width = (files.len() as f64).log10().ceil();
        println!("Width for {} files:\t{}", files.len(), width);
        for f in files {
            println!("File:\t{:?}", f);
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
