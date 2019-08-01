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

/// Common Unix Socket
pub const SOCK: &str = "/tmp/chat_path";

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

fn input_thread(chan: mpsc::Sender<String>) -> Result<(), mpsc::SendError<String>> {
    loop {
        let mut buf: String = String::new();
        match stdin().read_line(&mut buf) {
            Ok(0) => {
                // EOF
                return Ok(());
            }
            Ok(_) => {
                if buf != "".to_owned() {
                    chan.send(buf).unwrap();
                }
            }
            Err(_) => {
                continue;
            }
        }
    }
}


/// Spawn slave process in new terminal.\
/// Start Unix server (common::SOCK) & return stream
fn slaveinit() -> GenericResult<LineWriter<UnixProtect>> {
    let pipe = UnixListener::bind(SOCK).expect("BIND");
    Command::new("x-terminal-emulator")
        .arg("-e")
        .arg(
            env::current_exe()?
                .to_str()
                .ok_or("Can't do convert `current exe`")?,
        )
        .arg("-s")
        .spawn()?;

    let (sock, _) = pipe.accept().expect("ACCEPT");
    return Ok(LineWriter::new(UnixProtect::new(sock)));
}

fn readnick() -> io::Result<String> {
    print!("Nick? ");
    io::stdout().flush()?;
    loop {
        let mut buf = String::new();
        stdin().read_line(&mut buf)?;
        match buf.split_ascii_whitespace().nth(0) {
            Some(word) => {
                if word == "" {
                    continue;
                }
                return Ok(word.to_string());
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
) -> GenericResult<()> {
    let mut pending = false;
    let mut buf = String::from(format!("{}\n", CLI_SEND));
    for line in chan.try_recv() {
        pending = true;
        write!(display, "{} -> {}", nick, line)?;
        write!(buf, "{}", line)?;
    }
    if pending {
        write!(remote_write, "{}\n{}\n", buf, END_DELIM)?;
        println!("{}\n{}\n", buf, END_DELIM);
    }
    Ok(())
}

/// RECV all pending messages
fn recv(
    display: &mut LineWriter<UnixProtect>,
    remote_write: &mut LineWriter<TcpStream>,
    remote_read: &mut BufReader<TcpStream>,
) -> GenericResult<()> {
    write!(remote_write, "{}\n", CLI_RECV)?;
    let lines = remote_read
        .lines()
        .filter_map(|x| x.ok())
        .take_while(|l| l != END_DELIM)
        .filter(|l| l != "");
    for line in lines {
        write!(display, "{}", line)?;
    }
    Ok(())
}

/// Takes input and forwards to slave
fn master(port: String) -> GenericResult<()> {
    let nick = readnick()?;

    let sock = TcpStream::connect(&format!("localhost:{}", port))?;
    let sock2 = sock.try_clone()?;
    let mut remote_write = LineWriter::new(sock);
    let mut remote_read = BufReader::new(sock2);
    writeln!(remote_write, "{}", nick)?; // TODO retry on refused nick
    let mut display = slaveinit()?;

    let (tx, rx) = mpsc::channel::<String>();
    thread::spawn(move || {
        input_thread(tx).unwrap();
    });

    loop {
        send(&nick, &rx, &mut display, &mut remote_write)?;
        recv(&mut display, &mut remote_write, &mut remote_read)?;
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

fn main() -> GenericResult<()> {
    match env::args().nth(1) {
        Some(x) => {
            if x == "-s" {
                slave();
            } else {
                master(x)?;
            }
        }
        None => {
            master(PORT.to_string())?;
        }
    }
    Ok(())
}
