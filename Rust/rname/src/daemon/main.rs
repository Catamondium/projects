extern crate inotify;
use inotify::{Inotify, WatchDescriptor, WatchMask};

extern crate daemonize;
use daemonize::Daemonize;

use std::collections::HashMap;
use std::env;
use std::error::Error;
use std::path::*;

use std::fs::File;
use std::io::{BufRead, BufReader};

mod rname;
use rname::*;

type WatchMap = HashMap<WatchDescriptor, PathBuf>;

const CONF: Config = Config {
    verbose: false,
    dry: false,
    recurse: false,
};

const BUFSIZE: usize = 1024;

fn file_expand(args: &Vec<String>) -> std::io::Result<Vec<PathBuf>> {
    let mut ret = Vec::new();
    for a in args {
        let file = File::open(a)?;
        for line in BufReader::new(file).lines() {
            ret.push(Path::new(&line?).canonicalize()?);
        }
    }
    Ok(ret)
}

fn daemon_call(paths: &Vec<PathBuf>) -> Result<(), Box<dyn Error>> {
    let mut inotify = Inotify::init()?;

    let mut map: WatchMap = HashMap::new();
    for p in paths {
        let c = p.clone();
        let wd = inotify.add_watch(p,  WatchMask::MOVED_TO | WatchMask::MOVED_FROM | WatchMask::CREATE | WatchMask::DELETE)?;
        map.insert(wd, c);
    }

    loop {
        let mut buf = [0; BUFSIZE];
        let events = inotify.read_events_blocking(&mut buf)?;

        for e in events {
            let caller = map.get(&e.wd)?;
            println!("Event: {:?}", e.mask);
            println!("name: {:?}", caller);

            mv(&caller, &CONF)?;
        }
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let paths = file_expand(&env::args().skip(1).collect())?;

    match Daemonize::new().start() {
        Ok(_) => {
            daemon_call(&paths)?;
        }
        Err(e) => {
            println!("Error: {}", e);
        }
    }

    Ok(())
}
