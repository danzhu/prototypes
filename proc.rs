use std::{
    error,
    io::{self, Read, Write},
    process::{Command, Stdio},
    sync::mpsc::{self, Sender},
    thread,
};

#[derive(Clone)]
enum Source {
    Stdin,
    Command,
}

struct Event {
    src: Source,
    kind: EventKind,
}

enum EventKind {
    Read { buffer: Vec<u8> },
    Eof,
    Exit { status: Option<i32> },
}

struct Action {
    kind: ActionKind,
}

enum ActionKind {
    Write { buffer: Vec<u8> },
    Eof,
}

fn read_to_channel<T>(src: Source, mut stream: T, event_t: Sender<Event>)
where
    T: Read + Send + 'static,
{
    thread::spawn(move || {
        let mut buf = [0; 256];
        loop {
            let kind = match stream.read(&mut buf) {
                Ok(0) => break,
                Ok(read) => EventKind::Read {
                    buffer: buf[..read].to_owned(),
                },
                Err(err) => match err.kind() {
                    io::ErrorKind::Interrupted => continue,
                    _ => panic!("read: {}", err),
                },
            };
            let msg = Event {
                src: src.clone(),
                kind,
            };
            event_t.send(msg).expect("send");
        }
        let msg = Event {
            src,
            kind: EventKind::Eof,
        };
        event_t.send(msg).expect("send");
    });
}

fn write_from_channel<T>(mut stream: T) -> Sender<Action>
where
    T: Write + Send + 'static,
{
    let (action_t, action_r) = mpsc::channel();
    thread::spawn(move || loop {
        let Action { kind } = action_r.recv().expect("recv");
        match kind {
            ActionKind::Write { buffer } => {
                stream.write(&buffer).expect("write");
            }
            ActionKind::Eof => break,
        }
    });
    action_t
}

fn create_proc(mut cmd: Command, event_t: Sender<Event>) -> Sender<Action> {
    let mut proc = cmd
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .expect("spawn");
    let stdin = proc.stdin.take().expect("no stdin");
    let stdout = proc.stdout.take().expect("no stdout");
    let src = Source::Command;
    read_to_channel(src.clone(), stdout, event_t.clone());
    let action_t = write_from_channel(stdin);
    thread::spawn(move || {
        let status = proc.wait().expect("wait").code();
        let msg = Event {
            src,
            kind: EventKind::Exit { status },
        };
        event_t.send(msg).expect("send");
    });
    action_t
}

fn main() -> Result<(), Box<error::Error>> {
    let cmd = Command::new("cat");
    let (event_t, event_r) = mpsc::channel();
    read_to_channel(Source::Stdin, io::stdin(), event_t.clone());
    let action_t = create_proc(cmd, event_t);
    while let Ok(Event { src, kind }) = event_r.recv() {
        match src {
            Source::Stdin => match kind {
                EventKind::Eof => {
                    let msg = Action {
                        kind: ActionKind::Eof,
                    };
                    action_t.send(msg).expect("send");
                }
                EventKind::Read { buffer } => {
                    let msg = Action {
                        kind: ActionKind::Write { buffer },
                    };
                    action_t.send(msg).expect("send");
                }
                EventKind::Exit { .. } => unreachable!(),
            },
            Source::Command => match kind {
                EventKind::Eof => {}
                EventKind::Read { buffer } => {
                    io::stdout().write(&buffer).expect("write");
                }
                EventKind::Exit { status } => {
                    println!("{:?}", status);
                }
            },
        }
    }
    Ok(())
}
