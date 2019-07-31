use common::*;
use std::env;
use std::fmt::Write as fwrite;
use std::fs::remove_file;
use std::io::{stdin, BufRead, BufReader, LineWriter, Read, Write};
use std::net::TcpStream;
use std::os::unix::net::{UnixListener, UnixStream};
use std::process::{exit, Command};
use std::sync::mpsc;
use std::thread;


/* TODO
 * proper UnixStream handling -> insure deletion under panic! and normal exit
 * * wrap UnixStream in Dropping & Writer type?
*/

fn input_thread(chan: mpsc::Sender<String>) -> ! {
    loop {
        let mut buf: String = String::new();

        match stdin().read_line(&mut buf) {
            Ok(0) => {
                // EOF
                break;
            }
            Ok(_) => {
                chan.send(buf).unwrap();
            }
            Err(_) => {
                continue;
            }
        }
    }
    teardown();
}


/// Spawn slave process in new terminal.\
/// Start Unix server (common::SOCK) & return stream
fn slaveinit() -> LineWriter<UnixStream> {
    let pipe = UnixListener::bind(SOCK).expect("BIND");

    Command::new("x-terminal-emulator")
        .arg("-e")
        .arg(env::current_exe().unwrap().to_str().unwrap())
        .arg("-s")
        .spawn()
        .unwrap();

    let (sock, _) = pipe.accept().expect("ACCEPT");
    return LineWriter::new(sock);
}

fn teardown() -> ! {
    remove_file(SOCK).expect("Failed to close UNIX socket");
    exit(0);
}

fn readnick() -> String {
    print!("Nick? ");
    loop {
        let mut buf = String::new();
        stdin().read_line(&mut buf).unwrap();
        match buf.split_ascii_whitespace().nth(1) {
            Some(word) => {
                if word == "" {
                    continue;
                }
                return word.to_string();
            }

            None => {
                continue;
            }
        }
    }

}

// SEND all pending messages
fn send(
    nick: &str,
    chan: &mpsc::Receiver<String>,
    display: &mut LineWriter<UnixStream>,
    remote_write: &mut LineWriter<TcpStream>,
) {
    let mut pending = false;
    let mut buf = String::from(COM_SEND);
    for line in chan.try_iter() {
        write!(display, "{} -> {}", nick, line).unwrap();
        write!(&mut buf, "{}", line).unwrap();

        pending = true;
    }

    if pending {
        write!(remote_write, "{}", buf).unwrap();
    }

}

// RECV all pending messages
fn recv(
    display: &mut LineWriter<UnixStream>,
    remote_write: &mut LineWriter<TcpStream>,
    remote_read: &mut BufReader<TcpStream>,
) {
    write!(remote_write, "{}", COM_RECV).unwrap();
    let mut buf = String::new();
    remote_read.read_to_string(&mut buf).unwrap();
    write!(display, "{}", buf).unwrap();
}

/// Takes input and forwards to slave
fn master(port: String) {
    let nick = readnick();

    let sock = TcpStream::connect(&format!("localhost:{}", port)).unwrap();
    let sock2 = sock.try_clone().unwrap();
    let mut remote_write = LineWriter::new(sock);
    let mut remote_read = BufReader::new(sock2);
    writeln!(remote_write, "{}", nick).unwrap(); // TODO retry on refused nick
    let mut display = slaveinit();

    let (tx, rx) = mpsc::channel::<String>();
    thread::spawn(move || {
        input_thread(tx);
    });

    loop {
        send(&nick, &rx, &mut display, &mut remote_write);
        recv(&mut display, &mut remote_write, &mut remote_read);
    }
}

/// Writes everything recieved on common::SOCK to stdout
fn slave() {
    let pipe = UnixStream::connect(SOCK).unwrap();
    let reader = BufReader::new(pipe);

    for line in reader.lines().filter_map(|x| x.ok()) {
        println!("{}", line);
    }
}

fn main() {
    match env::args().nth(1) {
        Some(x) => {
            if x == "-s" {
                slave();
            } else {
                master(x);
            }
        }

        None => {
            master(PORT.to_string());
        }
    }
}
