use common::*;
use std::env;
use std::fmt::Write as fwrite;
use std::fs::remove_file;
use std::io::{self, stdin, BufRead, BufReader, BufWriter, LineWriter, Write};
use std::net::TcpStream;
use std::os::unix::net::{SocketAddr, UnixListener, UnixStream};
use std::process::Command;
use std::sync::mpsc;
use std::thread;

/// UnixStream wrapper providing Drop\
/// UnixStream is wrapped in BufWriter to delegate Write impl
struct UnixProtect {
    inner: BufWriter<UnixStream>,
    addr: SocketAddr,
}

impl UnixProtect {
    fn new(strm: UnixStream) -> Self {
        let addr = strm.local_addr().expect("Unnamed socket");
        let inner = BufWriter::new(strm);
        Self { addr, inner }
    }
}

impl Drop for UnixProtect {
    /// Automatically removes socket file
    fn drop(&mut self) {
        if !self.addr.is_unnamed() {
            let _ = remove_file(self.addr.as_pathname().unwrap());
        }
    }
}

impl Write for UnixProtect {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.inner.write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.inner.flush()
    }
}

fn input_thread(chan: mpsc::Sender<String>) {
    loop {
        let mut buf: String = String::new();

        match stdin().read_line(&mut buf) {
            Ok(0) => {
                // EOF
                return;
            }
            Ok(_) => {
                println!("PENDING");
                chan.send(buf).unwrap();
            }
            Err(_) => {
                continue;
            }
        }
    }
}


/// Spawn slave process in new terminal.\
/// Start Unix server (common::SOCK) & return stream
fn slaveinit() -> LineWriter<UnixProtect> {
    let pipe = UnixListener::bind(SOCK).expect("BIND");
    Command::new("x-terminal-emulator")
        .arg("-e")
        .arg(env::current_exe().unwrap().to_str().unwrap())
        .arg("-s")
        .spawn()
        .unwrap();

    let (sock, _) = pipe.accept().expect("ACCEPT");
    return LineWriter::new(UnixProtect::new(sock));
}

fn readnick() -> String {
    print!("Nick? ");
    io::stdout().flush().unwrap();
    loop {
        let mut buf = String::new();
        stdin().read_line(&mut buf).unwrap();
        match buf.split_ascii_whitespace().nth(0) {
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
    display: &mut LineWriter<UnixProtect>,
    remote_write: &mut LineWriter<TcpStream>,
) {
    let mut pending = false;
    let mut buf = String::from(format!("{}\n", COM_SEND));
    for line in chan.try_iter() {
        write!(display, "{} -> {}", nick, line).unwrap();
        write!(&mut buf, "{}", line).unwrap();

        pending = true;
    }

    if pending {
        println!("REMOTE SEND");
        write!(remote_write, "{}\n{}", buf, END_DELIM).unwrap();
    }

}

// TODO: STICKY
/// RECV all pending messages
fn recv(
    display: &mut LineWriter<UnixProtect>,
    remote_write: &mut LineWriter<TcpStream>,
    remote_read: &mut BufReader<TcpStream>,
) {
    write!(remote_write, "{}\n", COM_RECV).unwrap();
    let lines = remote_read
        .lines()
        .filter_map(|x| x.ok())
        .take_while(|l| l != "END");
    for line in lines {
        write!(display, "{}", line).unwrap();
    }
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
