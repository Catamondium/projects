use common::*;
use std::collections::HashMap;
use std::fmt::Write as fwrite;
use std::io::{BufRead, BufReader, LineWriter, Write};
use std::net::{TcpListener, TcpStream};
use std::sync::{Arc, LockResult, RwLock, RwLockWriteGuard};
use std::thread;

/// Active user hash
struct Connection(Arc<RwLock<HashMap<String, String>>>);

impl Connection {
    fn new() -> Self {
        Connection(Arc::new(RwLock::new(HashMap::new())))
    }

    fn clone(&self) -> Self {
        let cpy = self.0.clone();
        Connection(cpy)
    }

    fn write(&self) -> LockResult<RwLockWriteGuard<HashMap<String, String>>> {
        self.0.write()
    }
}

/// Recieve content from client
fn recv(reader: &mut BufReader<TcpStream>, nick: &str, conn: &mut Connection) {
    let concat = reader
        .lines()
        .filter_map(|x| x.ok())
        .take_while(|l| l != END_DELIM)
        .filter(|l| l != "")
        .map(|l| format!("{} -> {}", nick, l))
        .collect::<Vec<String>>()
        .join("\n");


    conn.write()
        .unwrap()
        .iter_mut()
        .filter(|(name, _)| *name != nick)
        .for_each(|(_, peer)| {
            write!(peer, "{}", concat).unwrap();
        });
}

/// Send content to client
fn send(
    writer: &mut LineWriter<TcpStream>,
    nick: &str,
    conn: &mut Connection,
) -> GenericResult<()> {
    let mut writeaccess = conn.write().unwrap();
    let mycontent = writeaccess.get_mut(nick).ok_or("Failed to get")?;
    write!(writer, "{}\n{}\n", mycontent, END_DELIM)?;
    mycontent.clear();
    println!("SEND");
    Ok(())
}

fn serv_loop(
    mut reader: BufReader<TcpStream>,
    mut writer: LineWriter<TcpStream>,
    mut conn: Connection,
) -> GenericResult<()> {
    // Register
    let mut nick = String::new();
    reader.read_line(&mut nick)?;
    println!("REG: {}", nick);
    conn.write().unwrap().insert(nick.clone(), String::new());

    let inner = reader.into_inner();

    let mut reader2 = BufReader::new(inner.try_clone()?);
    let lines = BufReader::new(inner).lines().filter_map(|x| x.ok());

    /* reader2 & lines trampling??
     *
     * did reader2 steal Tcp ctrl, blocking reciept of command?
     *  maybe false: command loop sees reader2's input
     *  maybe true: reader2 sees command's input (where no terminators occur)
     *
     *  sol 1: separate command and gen purpose channels
     *   other clients indistinguishable, no
     *  sol 2: duplexing struct?
     *   how, w/o duplicating handle?
     *    Rc<TcpStream> ?
     *     would have to have strict buffering behaviors
     *     or more stealing will happen
     *
     *     closest implementation would read byte-for-byte
     *     would keep lines representable, but essentially unbuffered
     *
     *  sol 3: 1 TcpStream-reader, 2 alternative readers
     *   retains buffering
     *   only 1 handle
     *   conditionally select reciever
     *
     * readers & writer
     * readers manage to avoid reading in writers transmissions
     */

    for line in lines.filter(|l| l != "") {
        if line == SER_SEND {
            send(&mut writer, &nick, &mut conn)?;
            println!("SEND");
        } else if line == SER_RECV {
            recv(&mut reader2, &nick, &mut conn);
            println!("RECV");
        } else {
            println!("invalid: {}", line);
        }
    }

    // Deregister
    conn.write().unwrap().remove(&nick);
    println!("DEREG: {}", nick);
    Ok(())
}

fn main() -> GenericResult<()> {
    let glo_conn = Connection::new();
    let server = TcpListener::bind(&format!("localhost:{}", PORT))?;

    for sock in server.incoming().filter_map(|x| x.ok()) {
        println!("Connection on: {:?}", sock.local_addr());
        let sock2 = sock.try_clone()?;
        let writer = LineWriter::new(sock);
        let reader = BufReader::new(sock2);
        let conn = glo_conn.clone();

        thread::spawn(move || {
            serv_loop(reader, writer, conn).unwrap();
        });
    }
    Ok(())
}
