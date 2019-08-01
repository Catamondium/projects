use std::error::Error;

pub const PORT: &str = "22588";
pub const SOCK: &str = "/tmp/chat_path";
pub const COM_SEND: &str = "SEND";
pub const COM_RECV: &str = "RECV";
pub const END_DELIM: &str = "END";
pub type GenericResult<T> = Result<T, Box<dyn Error>>;
