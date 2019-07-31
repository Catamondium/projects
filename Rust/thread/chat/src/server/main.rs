/* TODO
 * load nick
 * register User
 * loop COM:
 *  SEND
 *      Append to all **Others** tosend buffers
 *  RECV
 *      flush **User** buffer down socket
 * user EOF
 *  deregister User
*/

use common::*;
use std::collections::HashMap;
use std::fmt::Write as fwrite;
use std::io::{BufRead, BufReader, LineWriter, Read, Write};
use std::net::TcpListener;
use std::sync::{Arc, RwLock};
use std::thread;

struct Connection(Arc<RwLock<HashMap<String, String>>>);

impl Connection {
    fn new() -> Self {
        Connection(Arc::new(RwLock::new(HashMap::new())))
    }

    fn clone(&self) -> Self {
        let cpy = self.0.clone();
        Connection(cpy)
    }
}

fn main() {
    let glo_conn = Connection::new();
    let server = TcpListener::bind(&format!("localhost:{}", PORT)).unwrap();

    for sock in server.incoming().filter_map(|x| x.ok()) {
        let sock2 = sock.try_clone().unwrap();
        let writer = LineWriter::new(sock);
        let reader = BufReader::new(sock2);
        let conn = glo_conn.clone();

        thread::spawn(move || {
            let mut writer = writer;
            let mut reader = reader;
            let mut conn = conn;

            let mut nick = String::new();
            reader.read_line(&mut nick).unwrap();
            conn.0.get_mut().unwrap().insert(nick, String::new());

            for line in reader.lines().filter_map(|x| x.ok()) {
                if line == COM_SEND {
                    let mut buf = Vec::new();
                    reader.read_until(b'\r', &mut buf).unwrap();
                    conn.0.get_mut().unwrap().iter_mut().filter(|(name, _)| { *name != &nick}).for_each(|(_, peer)| {
                        write!(peer, "{}", std::str::from_utf8(&buf).unwrap()).unwrap();
                    })

                } else if line == COM_RECV {
                    let mycontent = conn.0.get_mut().unwrap().get_mut(&nick).unwrap();
                    write!(writer, "{}\r", mycontent).unwrap();
                    mycontent.clear();
                }
            }
        });
    }
}
