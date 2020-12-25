use super::common;
use super::cpu::instr::{Instructions};
use crate::cpu::Cpu;

pub fn run() {
    println!("Part 1 result: {}", part1());
}

fn part1() -> i64 {
    let prog = Instructions::from_input(common::read_file("src/day8/problem-input.txt").as_str());
    let mut cpu = Cpu::new(prog);

    // Add pre-instruction hook to trigger cpu halt before any instruction is re-executed
    cpu.set_pre_exec_hook(move |x| if x.get_next_instr().get_exec_count() == 1 { x.set_active(false); });
    cpu.execute();

    cpu.get_state().get_acc()
}
