pub mod common;
pub mod instr;
use common::*;
use instr::{Instr, Op};
use std::sync::mpsc;
use std::sync::mpsc::{Sender, Receiver};

pub struct Cpu {
    state: CpuState,
    hooks: CpuHooks
}

pub struct CpuState {
    prog: Vec<Instr>,
    active: bool,
    ip: usize,
    last_ip: Option<usize>,
    ip_adj: i64,                // End-of-cycle IP adjustment; allows JMP prior to 0 as long as IP increment will return to valid 0+ range at cycle end
    acc: Num,

    executed: usize,
    log_exec: bool,
}

struct CpuHooks {
    pre_exec: Box<dyn Fn(&mut CpuState)>,
    post_exec: Box<dyn Fn(&mut CpuState)>,

    halt_trigger: (Sender<()>, Receiver<()>)
}

impl Cpu {
    pub fn new(prog: Vec<Instr>) -> Self {
        Self {
            state: CpuState::new(prog),
            hooks: CpuHooks::new()
        }
    }

    pub fn execute(&mut self) {
        self.state.set_active(true);
        while self.state.is_active() {
            self.exec_next_instr();

            // Check for external halt trigger
            if self.should_halt() { self.halt(); }
        }
    }

    pub fn halt(&mut self) {
        self.state.set_active(false);
    }

    pub fn set_pre_exec_hook(&mut self, f: impl Fn(&mut CpuState) + 'static) {
        self.hooks.pre_exec = Box::new(f);
    }

    pub fn set_post_exec_hook(&mut self, f: impl Fn(&mut CpuState) + 'static) {
        self.hooks.post_exec = Box::new(f);
    }

    fn get_next_instr(&self) -> &Instr {
        self.state.get_next_instr()
    }

    fn get_last_instr(&self) -> Option<&Instr> {
        self.state.get_last_instr()
    }

    fn get_instr(&self, ip: usize) -> &Instr {
        self.state.get_instr(ip)
    }

    fn _get_instr_mut(&mut self, ip: usize) -> &mut Instr {
        self.state._get_instr_mut(ip)
    }

    fn exec_next_instr(&mut self) {
        self.exec_instr(self.state.ip)
    }

    fn exec_instr(&mut self, ip: usize) {
        // Pre-execution hook
        (self.hooks.pre_exec)(&mut self.state);
        if !self.state.active { return; }   // Recheck in case hook fn results in program halt

        // Execute instruction
        self.exec_instr_op(ip);

        // Post-execution hook (before IP is incremented)
        (self.hooks.post_exec)(&mut self.state);

        // Increment IP
        self.end_of_cycle_inc_ip();
    }

    fn exec_instr_op(&mut self, ip: usize) {
        let instr = self.get_instr(ip).clone();
        if self.state.log_exec {
            println!("Exec {}: {:?} [ip: {}, acc: {}, exec_n: {}]", ip, instr, self.state.ip, self.state.acc, self.state.executed);
        }

        match instr.get_op() {
            Op::NOP => (),
            Op::ACC(n) => self.acc(*n),
            Op::JMP(n) => self.jmp(*n),
            op => panic!("Unsupported opcode '{:?}'", op)
        }

        self.inc_exec_count();
        self.inc_instr_exec_count(ip);
    }

    fn acc(&mut self, n: Num) {
        self.state.acc += n;
    }

    fn jmp(&mut self, n: Addr) {
        self.inc_ip_by(n);
        self.set_eoc_ip_adj(-1);  // Add end-of-cycle IP adjustment of -1 since IP will otherwise increment at EOC before executing the jmp target
    }

    fn end_of_cycle_inc_ip(&mut self) {
        self.inc_ip_by(1 + self.state.ip_adj);  // Apply end-of-cycle IP adjustment to account for potential jmp instructions
        self.state.ip_adj = 0;
    }

    fn inc_ip_by(&mut self, n: Addr) {
        let new_ip = (self.state.ip as Addr) + n;
        assert!(new_ip >= 0, "Invalid IP adjustment (ip: {}, adj: {}, would result in new ip = {}", self.state.ip, n, new_ip);

        self.state.last_ip.replace(self.state.ip);
        self.state.ip = new_ip as usize;
    }

    fn set_eoc_ip_adj(&mut self, n: i64) {
        self.state.set_eoc_ip_adj(n);
    }

    fn inc_exec_count(&mut self) {
        self.state.executed += 1;
    }

    fn inc_instr_exec_count(&mut self, ip: usize) {
        self.state.prog.get_mut(ip)
            .unwrap_or_else(|| panic!("Cannot inc instruction exec count; invalid ip: {}", ip))
            .record_exec();
    }

    pub fn create_halt_trigger(&self) -> Sender<()> {
        self.hooks.create_halt_trigger()
    }

    fn should_halt(&self) -> bool {
        self.hooks.halt_trigger.1.try_recv()
            .map(|_| true)
            .unwrap_or_else(|e| false)
    }

    #[allow(dead_code)]
    fn set_execution_logging(&mut self, active: bool) {
        self.state.set_execution_logging(active);
    }
}

impl CpuState {
    pub fn new(prog: Vec<Instr>) -> Self {
        Self {
            prog,
            active: false,
            ip: 0,
            last_ip: None,
            ip_adj: 0,
            acc: 0,

            executed: 0,
            log_exec: false
        }
    }

    pub fn is_active(&self) -> bool { self.active }
    pub fn get_ip(&self) -> usize { self.ip }
    pub fn get_acc(&self) -> Num { self.acc }
    pub fn get_exec_count(&self) -> usize { self.executed }

    pub fn set_active(&mut self, active: bool) {
        self.active = active;
    }

    fn get_next_instr(&self) -> &Instr {
        self.get_instr(self.ip)
    }

    fn get_last_instr(&self) -> Option<&Instr> {
        self.last_ip.map(|ip| self.get_instr(ip))
    }

    fn get_instr(&self, ip: usize) -> &Instr {
        self.prog.get(ip)
            .unwrap_or_else(|| panic!("Invalid instruction pointer ({})", ip))
    }

    fn _get_instr_mut(&mut self, ip: usize) -> &mut Instr {
        self.prog.get_mut(ip)
            .unwrap_or_else(|| panic!("Invalid instruction pointer ({})", ip))
    }

    fn set_execution_logging(&mut self, active: bool) {
        self.log_exec = active;
    }

    fn set_eoc_ip_adj(&mut self, n: i64) {
        self.ip_adj = n;
    }
}

impl CpuHooks {
    pub fn new() -> Self {
        Self {
            pre_exec: Box::new(|_| ()),
            post_exec: Box::new(|_| ()),

            halt_trigger: mpsc::channel()
        }
    }

    pub fn create_halt_trigger(&self) -> Sender<()> {
        self.halt_trigger.0.clone()
    }
}

#[cfg(test)]
mod tests {
    use std::thread;
    use std::time::Duration;
    use std::sync::mpsc;
    use std::sync::mpsc::{Sender, Receiver};
    use super::Cpu;
    use super::instr::{Op, Instr, Instructions};
    use crate::cpu::CpuState;

    #[test]
    fn test_basic_pre_exec_hook_loop_termination() {
        let prog = Instructions::from_input("nop +0\nacc +1\njmp +4\nacc +3\njmp -3\nacc -99\nacc +1\njmp -4\nacc +6");
        let mut cpu = Cpu::new(prog);

        // Cpu halt hook immediately before any instruction is re-executed
        cpu.set_pre_exec_hook(move |x: &mut CpuState| if x.get_next_instr().get_exec_count() == 1 { x.set_active(false); });

        cpu.execute();
        assert_eq!(5, cpu.state.get_acc());
    }

    #[test]
    fn test_post_exec_hook() {
        let prog = Instructions::from_input("nop +0\nacc +1\nacc +10\nnop +0");
        let mut cpu = Cpu::new(prog);

        // Post-execution hook should trigger a cpu halt immediately after the first ACC instruction
        cpu.set_post_exec_hook(move |x| match x.get_next_instr().get_op() {
            Op::ACC(_) => x.set_active(false),
            _ => ()
        });

        cpu.execute();
        assert_eq!(1, cpu.state.get_acc());
        assert_eq!(2, cpu.state.get_exec_count());
        assert_eq!(Some(&Instr::new_with_executions(Op::ACC(1), 1)), cpu.get_last_instr());
    }

    #[test]
    fn test_public_instr_access() {
        let prog = Instructions::from_input("acc +1\nacc +2\nacc +3");
        let mut cpu = Cpu::new(prog);

        cpu.set_pre_exec_hook(move |x| if x.get_ip() == 2 { x.set_active(false); });
        cpu.execute();

        assert_eq!(Some(&Instr::new_with_executions(Op::ACC(2), 1)), cpu.get_last_instr());
        assert_eq!(&Instr::new(Op::ACC(3)), cpu.get_next_instr());
    }

    #[test]
    fn test_pre_zero_jmp_with_eoc_adj() {
        let prog = Instructions::from_input("nop +0\njmp -1");  // Will jump to (0 - 1) == -1 before IP inc
        let mut cpu = Cpu::new(prog);

        cpu.set_pre_exec_hook(move |x| if x.get_exec_count() == 5 { x.set_active(false); }); // To avoid inf loop
        cpu.execute();  // CPU should execute fully to halt triggered by hook, without failing on jmp prior to zero address
    }

    #[test]
    fn test_external_thread_halt() {
        let prog = Instructions::from_input("nop +0\njmp -1");  // Will infinite-loop without external halt command

        let mut cpu = Cpu::new(prog);
        cpu.set_pre_exec_hook(move |_| thread::sleep(Duration::from_micros(100)));

        let halt_tx = cpu.create_halt_trigger();
        let halt_thread = thread::spawn(move || {
            thread::sleep(Duration::from_millis(500));
            halt_tx.send(());
        });

        cpu.execute();

        // Test will only successfully reach this point if the halt trigger was correctly processed at the start of a cpu cycle
        assert!(!cpu.state.active);
    }

    #[test]
    #[should_panic(expected = "Cannot parse invalid opcode 'abc'")]
    fn test_handle_unknown_opcode() {
        Instructions::from_input("abc +0");
    }

    #[test]
    #[should_panic(expected = "Unsupported opcode 'UNK'")]
    fn test_handle_unsupported_opcode() {
        let prog = vec![Instr::new(Op::UNK)];
        let mut cpu = Cpu::new(prog);

        cpu.execute();
    }
}