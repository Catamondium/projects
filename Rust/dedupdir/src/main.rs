extern crate getopts;
use getopts::Options;

use std::collections::hash_map::DefaultHasher;
use std::hash::Hasher;
use std::collections::{HashMap, BTreeSet};
use std::fs::{DirEntry, File};
use std::io::{ Read};
use std::path::{Path, PathBuf};

trait Consumer<T, R = ()> {
    fn consume(&mut self, item: T) -> R;
}

/// recursively walks 'dir', returning files to ret Consumer
fn walkdir<T>(dir: &Path, ret: &mut T, recurse: &bool) -> std::io::Result<()>
where
    T: Consumer<DirEntry, std::io::Result<()>>,
{
    if dir.is_file() {
        Ok(())
    } else {
        let entries = dir.read_dir()?;
        for entry in entries {
            if let Ok(file) = entry {
                let kind = file.metadata()?.file_type();
                if kind.is_file() {
                    ret.consume(file)?;
                } else {
                    walkdir(&file.path(), ret, recurse)?;
                }
            }
        }
        Ok(())
    }
}

fn hashfile(path: PathBuf) -> std::io::Result<u64> {
    let mut bytes = Vec::new();
    let mut hasher = DefaultHasher::new();
    let mut f = File::open(&path)?;
    f.read(&mut bytes)?; // Not good for large files, most efficient option
    /*
    Combinations tried:
        HashSet DefaultHasher
        HashSet MD5
        BtreeSet DefaultHasher
        Vec DefaultHasher

    No longer sure if we have the issue, or the Python version
    They both used md5, only big thing is that we're consuming entire files at once,
    don't trust this version
    */
    hasher.write(&bytes);
    Ok(hasher.finish())
}

struct Dedup {
    files: HashMap<u64, Vec<u64>>,
    dry: bool,
    deletions: u64
}

impl Dedup {
    fn new(dry: &bool) -> Self {
        Dedup {
            dry: dry.clone(),
            files: HashMap::new(),
            deletions: 0
        }
    }
}

impl Consumer<DirEntry, std::io::Result<()>> for Dedup {
    fn consume(&mut self, item: DirEntry) -> std::io::Result<()> {
        let fsize = item.metadata()?.len();
        let hash = hashfile(item.path())?;
        if let Some(bucket) = self.files.get_mut(&fsize) {
            if  bucket.contains(&hash) {
                if self.dry {
                    println!("REPEAT: {:?}", item.path());
                } else {
                    std::fs::remove_file(item.path())?;
                }
                self.deletions += 1;
            } else {
                bucket.push(hash);
            }
        } else {
            let mut set = Vec::new();
            set.push(hash);
            self.files.insert(fsize, set);
        }
        Ok(())
    }
}

fn main() -> std::io::Result<()> {
    let argv: Vec<String> = std::env::args().skip(1).collect();
    for arg in argv {
        let mut dedup = Dedup::new(&true);
        walkdir(&Path::new(&arg), &mut dedup, &true)?;
        println!("{}: {} deletions", arg, dedup.deletions);
    }
    Ok(())
}
