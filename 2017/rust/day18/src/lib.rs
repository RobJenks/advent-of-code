pub fn run() {
    println!("Part 1 result: {}", part1());
}

fn part1() -> isize {
    CPU::new(100, parse_instructions(common::read_file("day18/input.txt"))).by_ref()
        .take_while(|st| st.rcv_count == 0)
        .last()
        .unwrap()
        .out
}

pub struct CPU {
    reg : Vec<isize>,           // Registers
    instr: Vec<Instruction>,    // Instruction set
    ip: isize,                  // Instruction pointer

    state: CPUState
}

#[derive(Clone, Debug)]
pub struct CPUState {
    out: isize,                 // Last output frequency
    rcv: isize,                 // Last recovered output frequency
    rcv_count: usize            // The number of receive actions triggered
}

#[allow(non_camel_case_types)]
#[derive(Clone, Debug)]
pub enum Instruction {
    snd{x: RegVal},
    set{x: RegVal, y: RegVal},
    add{x: RegVal, y: RegVal},
    mul{x: RegVal, y: RegVal},
    mdo{x: RegVal, y: RegVal},
    rcv{x: RegVal},
    jgz{x: RegVal, y: RegVal}
}

#[derive(Clone, Debug)]
pub enum RegVal {
    Reg{x: char},
    Val{x: isize}
}

impl CPU {
    pub fn new(register_count: usize, instr: Vec<Instruction>) -> Self {
        Self { reg: vec![0isize; register_count], instr, ip: 0, state: CPUState { out: 0, rcv: 0, rcv_count: 0 } }
    }

    fn reg(&self, rv: &RegVal) -> isize {
        match *rv {
            RegVal::Val {x} => x,
            RegVal::Reg {x} => self.reg[(x as usize) - ('a' as usize)]
        }
    }

    fn reg_index(&self, rv: &RegVal) -> isize {
        match *rv {
            RegVal::Val {x} => x,
            RegVal::Reg {x} => (x as isize) - ('a' as isize)
        }
    }

    fn set(&mut self, reg: &RegVal, val: &RegVal) {
        let ri = self.reg_index(reg);
        self.reg[ri as usize] = self.reg(val);
    }

    fn set_val(&mut self, reg: &RegVal, val: isize) {
        let ri = self.reg_index(reg);
        self.reg[ri as usize] = val;
    }
}

impl Iterator for CPU {
    type Item = CPUState;
    fn next(&mut self) -> Option<Self::Item> {
        let instr = self.instr[self.ip as usize].clone();

        //println!("Executing instruction {:?} with reg: {:?}, ip: {}, out: {}, rcv: {}", instr, self.reg, self.ip, self.state.out, self.state.rcv);
        match &instr {
            Instruction::snd {x} => self.state.out = self.reg(&x),
            Instruction::set {x,y} => self.set(&x, &y),
            Instruction::add {x,y} => self.set_val(&x, self.reg(&x) + self.reg(&y)),
            Instruction::mul {x,y} => self.set_val(&x, self.reg(&x) * self.reg(&y)),
            Instruction::mdo {x,y} => self.set_val(&x, self.reg(&x) % self.reg(&y)),
            Instruction::rcv {ref x} if self.reg(&x) != 0 => { self.state.rcv = self.state.out; self.state.rcv_count += 1 },
            Instruction::rcv {x:_} => (),
            Instruction::jgz {ref x, ref y} if self.reg(&x) > 0 => self.ip += (self.reg(&y) - 1) as isize,  // -1 since we will increment next
            Instruction::jgz {x:_,y:_} => ()
        }

        self.ip += 1;
        if self.ip >= 0 && self.ip < self.instr.len() as isize { Some(self.state.clone()) } else { None }
    }
}

impl RegVal {
    pub fn new(s: &str) -> Self {
        match s.parse::<isize>() {
            Ok(x) => RegVal::Val {x},
            Err(_) => RegVal::Reg {x: s.chars().next().unwrap()}
        }
    }
}


fn parse_instructions(input: String) -> Vec<Instruction> {
    input.split("\n")
        .map(|s| s.split_whitespace().collect::<Vec<&str>>())
        .map(|s| match *s.first().unwrap() {
            "snd" => Instruction::snd{ x: RegVal::new(s[1]) },
            "set" => Instruction::set{ x: RegVal::new(s[1]), y: RegVal::new(s[2])},
            "add" => Instruction::add{ x: RegVal::new(s[1]), y: RegVal::new(s[2])},
            "mul" => Instruction::mul{ x: RegVal::new(s[1]), y: RegVal::new(s[2])},
            "mod" => Instruction::mdo{ x: RegVal::new(s[1]), y: RegVal::new(s[2])},
            "rcv" => Instruction::rcv{ x: RegVal::new(s[1]) },
            "jgz" => Instruction::jgz{ x: RegVal::new(s[1]), y: RegVal::new(s[2])},
            _ => panic!("Unknown instruction")
        })
        .collect::<Vec<Instruction>>()
}

#[cfg(test)]
mod tests {
    use super::{CPU, Instruction, RegVal, parse_instructions };

    #[test]
    fn test_execution() {
        assert_eq!(
            CPU::new(5, parse_instructions(common::read_file("tests.txt"))).by_ref()
                .take_while(|st| st.rcv_count == 0)
                .last()
                .unwrap()
                .out, 4);
    }

}