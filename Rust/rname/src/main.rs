extern crate regex;
use regex::{escape, Regex};

extern crate getopts;
use getopts::Options;

use std::io::prelude::*;
use std::path::*;
use std::{env, fs};

macro_rules! return_on_none {
    ($($ret:ident = $e:expr);+) => {
       $(
            if $e.is_none() {
                return;
            }

            let $ret = $e.unwrap_or(Default::default());
       )+
    };
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let (args, config) = target(&args);

    for x in args {
        if config.dry || config.force || verify(&x) {
            mv(Path::new(&x), &config);
        }
    }
}

fn verify(path: &str) -> bool {
    println!("About to rename inside:\t{}\nAre you sure?", path);
    let mut buf = [0; 1];
    let err = std::io::stdin().read(&mut buf).is_ok();
    let c = std::str::from_utf8(&buf).unwrap_or("n");

    err && c.to_lowercase() == "y"
}

fn re_sort<'a>(dir: &'a Vec<PathBuf>, parent: &str, width: &usize) -> Vec<&'a PathBuf> {
    let re_string = format!(r"^{}-\d{{{}}}.*$", escape(&parent), width);
    let re = Regex::new(&re_string).unwrap();

    let (mut matched, mut unmatched): (Vec<&PathBuf>, Vec<&PathBuf>) = dir.iter().partition(|e| {
        if let Some(thing) = e.file_name().and_then(|x| x.to_str()) {
            return re.is_match(thing);
        }
        return false;
    });

    matched.sort();
    matched.append(&mut unmatched);

    matched
}

fn is_directory(file: &fs::DirEntry) -> bool {
    file.file_type().map(|x| x.is_dir()).unwrap_or(false)
}

fn mv(rel_parent: &Path, conf: &Config) {
    if !rel_parent.is_dir() {
        return;
    }

    return_on_none! {parent = rel_parent.canonicalize().ok()};

    if let Ok(iter) = fs::read_dir(&parent) {
        let filtered = iter.filter_map(|x| x.ok());
        let (dirs, files): (Vec<fs::DirEntry>, Vec<fs::DirEntry>) =
            filtered.partition(is_directory);

        let fpaths: Vec<PathBuf> = files.iter().map(|f| f.path()).collect();
        let width = (fpaths.len() as f32).log10().ceil().abs() as usize;
        return_on_none! {dirname = parent.file_name().and_then(|x| x.to_str())};

        let sorted = re_sort(&fpaths, &dirname, &width);

        for (i, f) in sorted.iter().enumerate() {
            let ext = match f.extension() {
                Some(e) => format!("{}{}", ".", e.to_str().unwrap_or("")),
                None => String::new(),
            };

            let newname = format!("{}-{:03$}{}", dirname, i, ext, width);
            if conf.verbose {
                println!(
                    "{:?} -> \"{}\"",
                    f.file_name().and_then(|x| x.to_str()).unwrap_or("{ERR}"),
                    newname
                );
            }

            if !conf.dry {
                let _r = fs::rename(f, parent.join(Path::new(&newname)));
            }
        }

        if conf.recurse {
            for d in dirs {
                mv(&d.path(), &conf); // Don't recurse when testing
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Config {
    pub dry: bool,
    pub verbose: bool,
    pub recurse: bool,
    pub force: bool,
}

fn usage(program: &str, opts: &Options) {
    let brief = format!("Usage: {} [-dvrfih] [DIR]...\nBatch renamer following DIR-#.ext pattern.\n# is an integer zfilled to width log10 of the number of files to mv recursing directories", program);
    print!("{}", opts.usage(&brief));
}

fn target(args: &Vec<String>) -> (Vec<String>, Config) {
    let program = args[0].clone();

    let mut opts = Options::new();
    opts.optflag("d", "dry_run", "Run without renaming anything, verbosely");
    opts.optflag("v", "verbose", "Show all possible moves");
    opts.optflag("r", "recurse", "Recurse through subdirectories");
    opts.optflag("f", "force", "Don't ask for approval");
    opts.optflag("i", "interactive", "Require approval");
    opts.optflag("h", "help", "Show this usage");
    let matches = opts.parse(&args[1..]).unwrap_or_else(|e| {
        eprintln!("Bad option: {}", e);
        usage(&program, &opts);
        std::process::exit(1);
    });

    if matches.opt_present("h") {
        usage(&program, &opts);
        std::process::exit(0);
    }

    let conf = Config {
        dry: matches.opt_present("d"),
        verbose: matches.opt_present("d") || matches.opt_present("v"),
        recurse: matches.opt_present("r"),
        force: matches.opt_present("f") && !matches.opt_present("i"),
    };

    let mut ret_vec = matches.free;
    if ret_vec.len() == 0 {
        if let Ok(pwd) = env::var("PWD") {
            ret_vec.push(pwd);
        }
    }

    (ret_vec, conf)
}
