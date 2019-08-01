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
use std::io::{BufRead, BufReader, LineWriter, Write};
use std::net::{TcpListener, TcpStream};
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

fn send(reader: &mut BufReader<TcpStream>, nick: &str, conn: &mut Connection) {
    println!("SEND");
    let concat = reader
        .lines()
        .filter_map(|x| x.ok())
        .map(|l| format!("{} -> {}", nick, l))
        .take_while(|l| l != END_DELIM)
        .collect::<Vec<String>>()
        .join("");

    conn.0
        .write()
        .unwrap()
        .iter_mut()
        .filter(|(name, _)| *name != nick)
        .for_each(|(_, peer)| {
            write!(peer, "{}", concat).unwrap();
        });
}

fn recv(writer: &mut LineWriter<TcpStream>, nick: &str, conn: &mut Connection) {
    println!("RECV");
    let mut writeaccess = conn.0.write().unwrap();
    let mycontent = writeaccess.get_mut(nick).unwrap();
    write!(writer, "{}\n{}", mycontent, END_DELIM).unwrap();
    mycontent.clear();
}

fn serv_loop(
    mut reader: BufReader<TcpStream>,
    mut writer: LineWriter<TcpStream>,
    mut conn: Connection,
) {
    // Register
    let mut nick = String::new();
    reader.read_line(&mut nick).unwrap();
    println!("REG: {}", nick);
    conn.0.write().unwrap().insert(nick.clone(), String::new());

    // WARN, might not move each-other along
    let inner = reader.into_inner();

    let mut reader2 = BufReader::new(inner.try_clone().unwrap());
    let lines = BufReader::new(inner).lines().filter_map(|x| x.ok());

    for line in lines {
        if line == COM_SEND {
            send(&mut reader2, &nick, &mut conn);
        } else if line == COM_RECV {
            recv(&mut writer, &nick, &mut conn);
        } else {
            continue;
        }
    }

    // Deregister
    conn.0.write().unwrap().remove(&nick);
}

fn main() {
    let glo_conn = Connection::new();
    let server = TcpListener::bind(&format!("localhost:{}", PORT)).unwrap();

    for sock in server.incoming().filter_map(|x| x.ok()) {
        println!("Connection on: {:?}", sock.local_addr());
        let sock2 = sock.try_clone().unwrap();
        let writer = LineWriter::new(sock);
        let reader = BufReader::new(sock2);
        let conn = glo_conn.clone();

        thread::spawn(move || {
            serv_loop(reader, writer, conn);
        });
    }
}
