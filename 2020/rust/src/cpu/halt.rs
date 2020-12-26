pub const HALT_REASON_DIRECT: &str = "Direct halt command";
pub const HALT_REASON_SIGNAL: &str = "Received halt signal";
pub const HALT_REASON_END_OF_PROG: &str = "End of program";

#[derive(Debug, PartialEq)]
pub enum HaltCode {
    None,
    Normal(NormalHalt, String),
    Fault(FaultCondition, String)
}

#[derive(Debug, PartialEq)]
pub enum NormalHalt {
    Direct,
    Signal,
    EndOfProgram
}

#[derive(Debug, PartialEq)]
pub enum FaultCondition {
    UnsupportedOpcode
}