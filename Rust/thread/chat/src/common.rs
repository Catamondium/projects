use std::error::Error;

/// Default port
pub const PORT: &str = "22588";
/// Client sending invocation
pub const CLI_SEND: &str = "SEND";
/// Client recieving invocation
pub const CLI_RECV: &str = "RECV";
/// Server send request
pub const SER_SEND: &str = CLI_RECV;
/// Server recieve request
pub const SER_RECV: &str = CLI_SEND;
/// message terminator
pub const END_DELIM: &str = "END";
pub type GenericResult<T> = Result<T, Box<dyn Error>>;
