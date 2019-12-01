extern crate getopts;
use getopts::Options;

use std::hash::Hasher;
use std::collections::{HashMap, HashSet};
use std::collections::hash_map::DefaultHasher;
use std::fs::{DirEntry, File};
use std::path::{Path, PathBuf};
use std::io::{Read, BufReader, BufRead};

trait Consumer<T> {
    fn consume(&mut self, item: T);
}

/// recursively walks 'dir', returning files to ret Consumer
fn walkdir<T: Consumer<DirEntry>>(dir: &Path, ret: &mut T, recurse: &bool) -> std::io::Result<()> {
    if dir.is_file() {
        Ok(())
    } else {
        let entries = dir.read_dir()?;
        for entry in entries {
            if let Ok(file) = entry {
                let kind = file.metadata()?.file_type();
                if kind.is_file() {
                    ret.consume(file);
                } else {
                    walkdir(&file.path(), ret, recurse)?;
                }
            }
        }
        Ok(())
    }
}

fn hashfile(path: PathBuf) -> u64 {
    let mut hasher = DefaultHasher::new();
    let mut bytes = Vec::new();
    let mut f = File::open(&path).expect("Unopenable file");
    
    f.read(&mut bytes).expect("Unreadable file"); // Not good for large files, most efficient option
    hasher.write(&bytes);
    hasher.finish()
}

struct Dedup {
    files: HashMap<u64, HashSet<u64>>,
    dry: bool
}

impl Dedup {
    fn new(dry: bool) -> Self {
        Dedup {
            dry,
            files: HashMap::new()
        }
    }
}

impl Consumer<DirEntry> for Dedup {
    fn consume(&mut self, item: DirEntry) {
        let fsize = item.metadata().expect("Cannot meta").len();
        let hash = hashfile(item.path());
        if let Some(bucket) = self.files.get(&fsize) {
            if let Some(_) = bucket.get(&hash) {
                if self.dry {
                println!("REPEAT: {:?}", item.path());
                } else {
                    let _ = std::fs::remove_file(item.path());
                }
            }
        } else {
            let mut set = HashSet::new();
            set.insert(hash);
            self.files.insert(fsize, set);
        }
    }
}

fn main() -> std::io::Result<()> {
    let mut dedup = Dedup::new(true);
    let argv: Vec<String> = std::env::args().skip(1).collect();
    for arg in argv {
        walkdir(&Path::new(&arg), &mut dedup, &true)?;
    }
    Ok(())
}
