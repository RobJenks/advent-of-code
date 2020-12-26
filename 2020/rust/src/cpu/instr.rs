use itertools::Itertools;
use std::str::FromStr;
use std::fmt::Display;
use super::common::*;

pub type Prog = Vec<Instr>;

#[derive(Debug, Clone, PartialEq)]
pub enum Op {
    UNK,
    NOP(Num),
    ACC(Num),
    JMP(Addr)
}

#[derive(Debug, Clone, PartialEq)]
pub struct Instr {
    pub op: Op,
    exec_count: u32
}

impl Instr {
    pub fn new(op: Op) -> Self {
        Self { op, exec_count: 0 }
    }

    pub fn new_with_executions(op: Op, previous_executions: u32) -> Self {
        Self { op, exec_count: previous_executions }
    }

    pub fn get_op(&self) -> &Op { &self.op }
    pub fn get_exec_count(&self) -> u32 { self.exec_count }

    pub fn record_exec(&mut self) { self.exec_count += 1 }
}

pub struct Instructions { }
impl Instructions {
    pub fn from_input(input: &str) -> Prog {
        Self::from_lines(&input.lines()
            .map(|x| x.to_string())
            .collect())
    }

    pub fn from_lines(input: &Vec<String>) -> Prog {
        input.iter()
            .map(|x| Self::parse_instr(x))
            .collect()
    }

    pub fn parse_instr(s: &str) -> Instr {
        s.split_whitespace().collect_tuple::<(&str,&str)>()
            .map(|x| match x {
                ("nop", n) => Op::NOP(Self::parse_valid(n)),
                ("acc", n) => Op::ACC(Self::parse_valid(n)),
                ("jmp", n) => Op::JMP(Self::parse_valid(n)),
                (op, _) => panic!("Cannot parse invalid opcode '{}'", op)
            })
            .map(|op| Instr::new(op))
            .unwrap_or_else(|| panic!("Cannot parse instruction from '{}'", s))
    }

    fn parse_valid<T: FromStr>(s: &str) -> T
        where T: FromStr,
              T::Err: Display {

        s.parse::<T>()
            .unwrap_or_else(|x| panic!("Failed to parse '{}' ({})", s, x))
    }

}