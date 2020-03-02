use common::*;
use std::env;
use std::fmt::Write as fwrite;
use std::fs::remove_file;
use std::io::{self, stdin, BufRead, BufReader, LineWriter, Read, Write};
use std::net::TcpStream;
use std::os::unix::net::{UnixListener, UnixStream};
use std::process;
use std::process::Command;
use std::sync::mpsc;
use std::thread;

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
fn slaveinit() -> GenericResult<LineWriter<UnixStream>> {
    let addr = format!("/tmp/slave_{}", process::id());
    let _ = remove_file(addr.clone());
    let pipe = UnixListener::bind(addr.clone()).expect("BIND");
    Command::new("x-terminal-emulator")
        .arg("-e")
        .arg(
            env::current_exe()?
                .to_str()
                .ok_or("Can't do convert `current exe`")?,
        )
        .arg("-s")
        .arg(addr)
        .spawn()?;

    let (sock, _) = pipe.accept().expect("ACCEPT");
    return Ok(LineWriter::new(sock));
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
fn send<A: Write, B: Write>(
    nick: &str,
    chan: &mpsc::Receiver<String>,
    display: &mut LineWriter<A>,
    remote_write: &mut LineWriter<B>,
) -> GenericResult<()> {
    let mut pending = false;
    let mut buf = String::from(format!("{}\n", CLI_SEND));
    for line in chan.try_iter() {
        pending = true;
        write!(display, "{} -> {}\n", nick, line)?;
        write!(buf, "{}\n", line)?;
    }

    if pending {
        write!(remote_write, "{}\n{}\n", buf, END_DELIM)?;
        println!("SENT");
    }
    Ok(())
}

/// RECV all pending messages
fn recv<A: Write, B: Write, C: Read>(
    display: &mut LineWriter<A>,
    remote_write: &mut LineWriter<B>,
    remote_read: &mut BufReader<C>,
) -> GenericResult<()> {
    write!(remote_write, "{}\n", CLI_RECV)?;
    let lines = remote_read
        .lines()
        .filter_map(|x| x.ok())
        .take_while(|l| l != END_DELIM)
        .filter(|l| l != "");
    for (_, line) in lines.enumerate() {
        writeln!(display, "{}", line)?;
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

/// Display mode 'client' to avoid conflicts on STDOUT
fn slave(sock: &str) {
    let pipe = UnixStream::connect(sock).unwrap();
    let reader = BufReader::new(pipe);

    for line in reader.lines().filter_map(|x| x.ok()).filter(|l| l != "") {
        println!("{}", line);
    }
}

fn main() -> GenericResult<()> {
    match env::args().nth(1) {
        Some(x) => {
            if x == "-s" {
                slave(env::args().nth(2).expect("Unbound slave").as_str());
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
