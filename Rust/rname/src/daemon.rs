extern crate inotify;
use inotify::{Inotify, WatchDescriptor, WatchMask};

extern crate daemonize;
use daemonize::Daemonize;

use std::collections::HashMap;
use std::env;
use std::error::Error;
use std::path::*;

mod rname;
use rname::*;

type WatchMap = HashMap<WatchDescriptor, PathBuf>;

const CONF: Config = Config {
    verbose: false,
    dry: false,
    recurse: false,
};

fn to_path(args: &Vec<String>) -> Result<Vec<PathBuf>, Box<dyn Error>> {
    let mut vec = Vec::new();
    for x in args {
        vec.push(Path::new(&x).canonicalize()?);
    }

    Ok(vec)
}

fn main() -> Result<(), Box<dyn Error>> {
    let paths = to_path(&env::args().skip(1).collect())?;

    Daemonize::new().start()?;

    let mut inotify = Inotify::init()?;

    let mut map: WatchMap = HashMap::new();
    for p in paths {
        let c = p.clone();
        let wd = inotify.add_watch(p, WatchMask::CREATE | WatchMask::DELETE)?;
        map.insert(wd, c);
    }

    loop {
        let mut buf = [0; 1024];
        let events = inotify.read_events_blocking(&mut buf)?;

        for e in events {
            let caller = map.get(&e.wd).unwrap();
            println!("Event: {:?}", e.mask);
            println!("name: {:?}", caller);

            mv(&caller, &CONF)?;
        }
    }
}
