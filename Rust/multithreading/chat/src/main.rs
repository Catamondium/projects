use std::collections::HashMap;

use std::env;
use std::io;
use std::io::{BufRead, BufReader, LineWriter, Write};
use std::net::{TcpListener, TcpStream};


use std::sync::{Arc, RwLock};
use std::thread;


/*
 * Users map
 * get nick
 * read input
 * discard user
*/

struct Connections(Arc<RwLock<HashMap<String, LineWriter<TcpStream>>>>);
impl Connections {
    fn new() -> Self {
        Connections(Arc::new(RwLock::new(HashMap::new())))
    }
}

fn main() -> io::Result<()> {
    let port = env::args().nth(1).unwrap_or("8080".to_string());
    let conn = Connections::new();

    println!("Listening on: localhost:{}", port);
    TcpListener::bind(&format!("localhost:{}", port))?
        .incoming()
        .try_for_each(|socket| -> io::Result<()> {
            let socket = socket.unwrap();
            let conn = conn.0.clone();

            thread::spawn(move || {
                let (mut reader, writer) = socket
                    .try_clone()
                    .map(|socket1| (BufReader::new(socket1), LineWriter::new(socket)))
                    .unwrap();

                let mut nick = String::new();
                reader
                    .read_line(&mut nick)
                    .and_then(|_| Ok(nick.pop()))
                    .expect("Cannot read nick");
                conn.write().unwrap().insert(nick.clone(), writer);

                reader.lines().into_iter().for_each(|line| {
                    let line = line.expect("Cannot read input");
                    conn.write()
                        .unwrap()
                        .iter_mut()
                        .filter(|(other, _)| **other != nick)
                        .for_each(|(_, peer)| {
                            writeln!(peer, "{}\t->{}", nick, line).unwrap();
                        })
                });

                conn.write().unwrap().remove(&nick);
            });

            return Ok(());
        })?;

    return Ok(());
}
