pub const HALT_REASON_DIRECT: &str = "Direct halt command";
pub const HALT_REASON_SIGNAL: &str = "Received halt signal";
pub const HALT_REASON_END_OF_PROG: &str = "End of program";

#[derive(Debug, PartialEq)]
pub enum HaltCode {
    None,
    Normal(String),
    Fault(String)
}