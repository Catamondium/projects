extern crate regex;
use regex::{escape, Error as reError, Regex};

use std::error::Error;
use std::fs;
use std::path::{Path, PathBuf};
use std::string::ToString;

#[derive(Debug, Clone, Default)]
pub struct Config {
    pub dry: bool,
    pub verbose: bool,
    pub recurse: bool,
}

fn re_sort<'a>(
    dir: &'a Vec<PathBuf>,
    parent: &str,
    width: &usize,
) -> Result<Vec<&'a PathBuf>, reError> {
    let re_string = format!(r"^{}-\d{{{}}}.*$", escape(&parent), width);
    let re = Regex::new(&re_string)?;

    let (mut matched, mut unmatched): (Vec<&PathBuf>, Vec<&PathBuf>) = dir.iter().partition(|e| {
        if let Some(thing) = e.file_name().and_then(|x| x.to_str()) {
            return re.is_match(thing);
        }
        return false;
    });

    matched.sort();
    unmatched.sort();
    matched.append(&mut unmatched);

    Ok(matched)
}

#[inline]
fn is_directory(file: &fs::DirEntry) -> bool {
    file.file_type().map(|x| x.is_dir()).unwrap_or(false)
}

pub fn mv(rel_parent: &Path, conf: &Config) -> Result<(), Box<dyn Error>> {
    let parent = rel_parent.canonicalize()?;

    let iter = fs::read_dir(&parent)?;
    let filtered = iter.filter_map(|x| x.ok());
    let (dirs, files): (Vec<fs::DirEntry>, Vec<fs::DirEntry>) = filtered.partition(is_directory);

    let fpaths: Vec<PathBuf> = files.iter().map(|f| f.path()).collect();
    let width = fpaths.len().to_string().len();
    let dirname = parent
        .file_name()
        .and_then(|x| x.to_str())
        .ok_or("No Filename")?;

    let sorted = re_sort(&fpaths, &dirname, &width)?;

    for (i, f) in sorted.iter().enumerate() {
        let ext = match f.extension() {
            Some(e) => format!(".{}", e.to_str().unwrap_or("")),
            None => String::new(),
        };

        let newname = format!("{}-{:03$}{}", dirname, i, ext, width);
        let newpath = parent.join(Path::new(&newname));
        if newpath == **f {
            continue;
        }

        if conf.verbose {
            println!(
                "{:?} -> \"{}\"",
                f.file_name().and_then(|x| x.to_str()).unwrap_or("{ERR}"),
                newname
            );
        }

        if !conf.dry {
            fs::rename(f, newpath)?;
        }
    }

    if conf.recurse {
        for d in dirs {
            mv(&d.path(), &conf)?; // Don't recurse when testing
        }
    }

    Ok(())
}
