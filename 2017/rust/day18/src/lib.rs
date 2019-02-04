pub fn run() {
    println!("Part 1 result: {}", part1(common::read_file("day18/input.txt")));
    println!("Part 2 result: {}", part2(common::read_file("day18/input.txt")));
}

pub fn part1(input: String) -> isize {
    CPU::new(100, parse_instructions(input, APIVersion::V1)).by_ref()
        .take_while(|st| st.rcv_count == 0)
        .last()
        .unwrap()
        .out
}


pub fn part2(input: String) -> usize {
    run_multiple(
        &parse_instructions(input, APIVersion::V2), 100
    )[1].snd_count
}


fn run_multiple(instr: &Vec<Instruction>, reg_count: usize) -> Vec<CPUState> {
    let mut cpu = (0..2).map(|_| CPU::new(reg_count, instr.clone())).collect::<Vec<CPU>>();

    // cpu-id register 'p', unique to each instance
    cpu.iter_mut().enumerate().for_each(|(i, x)| x.set_val(&RegVal::Reg{x: 'p'}, i as isize));

    while (0..2).map(|i| {                         // Iterate while at least one CPU is active and not blocked
        match &cpu[i].by_ref().next() {
            Some(st) if st.sending => {                     // Transmit data ready in output pipe
                cpu[(i+1)%2].receive_value(st.out);
                cpu[i].state.sending = false;
                true
            },
            Some(st) if st.receiving => false,              // Blocking receive, no data available yet
            None => false,                                  // Execution terminated

            _ => true                                       // Any other instruction
        }
    }).any(|x| x) { }

    vec![cpu[0].state.clone(), cpu[1].state.clone()]
}


pub struct CPU {
    reg : Vec<isize>,           // Registers
    instr: Vec<Instruction>,    // Instruction set
    ip: isize,                  // Instruction pointer

    state: CPUState
}

#[derive(Clone, Debug)]
pub struct CPUState {
    pub last: Instruction,          // Copy of the last instruction executed
    pub out: isize,                 // Last output message
    pub rcv: Vec<isize>,            // Input message queue
    pub snd_count: usize,           // The number of send actions triggered
    pub rcv_count: usize,           // The number of receive actions triggered
    pub sending: bool,              // Indicates that the output pipe has sent a new value (through 'out'); set until cleared by caller
    pub receiving: bool,            // Indicates that the input pipe is waiting for a new value (through 'rcv')
}

#[allow(non_camel_case_types)]
#[derive(Clone, Debug)]
pub enum Instruction {
    nil,
    snd{x: RegVal, v: APIVersion},
    set{x: RegVal, y: RegVal},
    add{x: RegVal, y: RegVal},
    mul{x: RegVal, y: RegVal},
    mdo{x: RegVal, y: RegVal},
    rcv{x: RegVal, v: APIVersion},
    jgz{x: RegVal, y: RegVal},
    sub{x: RegVal, y: RegVal},
    jnz{x: RegVal, y: RegVal}
}

#[derive(Clone, Debug, PartialEq)]
pub enum APIVersion {
    V1,
    V2
}

#[derive(Clone, Debug)]
pub enum RegVal {
    Reg{x: char},
    Val{x: isize}
}

impl CPU {
    pub fn new(register_count: usize, instr: Vec<Instruction>) -> Self {
        Self { reg: vec![0isize; register_count], instr, ip: 0, state: CPUState {
            last: Instruction::nil, out: 0, rcv: vec![], snd_count: 0, rcv_count: 0, sending: false, receiving: false }
        }
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

    fn receive_value(&mut self, val: isize) {
        self.state.rcv.push(val);
    }
}

impl Iterator for CPU {
    type Item = CPUState;
    fn next(&mut self) -> Option<Self::Item> {
        let instr = self.instr[self.ip as usize].clone();
        self.state.last = instr.clone();

        //println!("Executing instruction {:?} with reg: {:?}, ip: {}, out: {}, rcv: {}", instr, self.reg, self.ip, self.state.out, self.state.rcv);
        match &instr {
            Instruction::set {x,y} => self.set(&x, &y),
            Instruction::add {x,y} => self.set_val(&x, self.reg(&x) + self.reg(&y)),
            Instruction::mul {x,y} => self.set_val(&x, self.reg(&x) * self.reg(&y)),
            Instruction::mdo {x,y} => self.set_val(&x, self.reg(&x) % self.reg(&y)),
            Instruction::jgz {ref x, ref y} if self.reg(&x) > 0 => self.ip += (self.reg(&y) - 1) as isize,  // -1 since we will increment next
            Instruction::jgz {x:_,y:_} => (),

            Instruction::snd {x, v} if *v == APIVersion::V1 => { self.state.out = self.reg(&x); self.state.snd_count += 1; },
            Instruction::snd {x, v} if *v == APIVersion::V2 => {
                if self.state.sending { panic!("Sending through output pipe before last value has been consumed; data loss"); }
                self.state.sending = true;
                self.state.snd_count += 1;
                self.state.out = self.reg(&x);
            },

            Instruction::rcv {ref x, v} if *v == APIVersion::V1 && self.reg(&x) != 0 => { self.state.rcv_count += 1 },
            Instruction::rcv {x:_, v} if *v == APIVersion::V1 => (),
            Instruction::rcv {x, v} if *v == APIVersion::V2 => {
                if self.state.rcv.is_empty() { self.state.receiving = true; return Some(self.state.clone()); }
                let val = self.state.rcv.remove(0);
                self.set_val(&x, val);
                self.state.receiving = false;
            },

            Instruction::sub {x,y} => self.set_val(&x, self.reg(&x) - self.reg(&y)),
            Instruction::jnz {ref x, ref y} if self.reg(&x) != 0 => self.ip += (self.reg(&y) - 1) as isize,  // -1 since we will increment next
            Instruction::jnz {x:_,y:_} => (),

            _ => panic!("Unknown instruction")
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


pub fn parse_instructions(input: String, api_version: APIVersion) -> Vec<Instruction> {
    input.split("\n")
        .map(|s| s.split_whitespace().collect::<Vec<&str>>())
        .map(|s| match *s.first().unwrap() {
            "snd" => Instruction::snd{ x: RegVal::new(s[1]), v: api_version.clone() },
            "set" => Instruction::set{ x: RegVal::new(s[1]), y: RegVal::new(s[2])},
            "add" => Instruction::add{ x: RegVal::new(s[1]), y: RegVal::new(s[2])},
            "mul" => Instruction::mul{ x: RegVal::new(s[1]), y: RegVal::new(s[2])},
            "mod" => Instruction::mdo{ x: RegVal::new(s[1]), y: RegVal::new(s[2])},
            "jgz" => Instruction::jgz{ x: RegVal::new(s[1]), y: RegVal::new(s[2])},
            "rcv" => Instruction::rcv{ x: RegVal::new(s[1]), v: api_version.clone() },
            "sub" => Instruction::sub{ x: RegVal::new(s[1]), y: RegVal::new(s[2])},
            "jnz" => Instruction::jnz{ x: RegVal::new(s[1]), y: RegVal::new(s[2])},
            _ => panic!("Unknown instruction")
        })
        .collect::<Vec<Instruction>>()
}

#[cfg(test)]
mod tests {
    use super::{CPU, Instruction, RegVal, APIVersion, parse_instructions, run_multiple };

    #[test]
    fn test_execution() {
        assert_eq!(
            CPU::new(5, parse_instructions(common::read_file("tests.txt"), APIVersion::V1)).by_ref()
                .take_while(|st| st.rcv_count == 0)
                .last()
                .unwrap()
                .out, 4);
    }
}