use super::common;
use crate::cpu::Cpu;
use crate::cpu::instr::{Instructions, Instr, Prog, Op};
use crate::cpu::halt::*;

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> i64 {
    let prog = Instructions::from_input(common::read_file("src/day8/problem-input.txt").as_str());
    let mut cpu = Cpu::new(prog);

    add_infinite_loop_halt_guard(&mut cpu);
    cpu.execute();

    cpu.get_state().get_acc()
}

fn part2() -> i64 {
    let prog = Instructions::from_input(common::read_file("src/day8/problem-input.txt").as_str());

    (0..prog.len())
        .filter_map(|ix| generate_prog_with_subst(&prog, ix))

        .filter_map(|p| {
            let mut cpu = Cpu::new(p);
            add_infinite_loop_halt_guard(&mut cpu);
            cpu.execute();

            match cpu.get_state().get_halt_code() {
                HaltCode::Normal(NormalHalt::EndOfProgram, _) => Some(cpu.get_state().get_acc()),
                _ => None
            }
        })
        .next()
        .unwrap_or_else(|| panic!("No results"))
}

fn add_infinite_loop_halt_guard(cpu: &mut Cpu) {
    // Add pre-instruction hook to trigger cpu halt before any instruction is re-executed
    cpu.set_pre_exec_hook(move |x| if x.get_next_instr().get_exec_count() == 1 { x.set_active(false); });
}

fn generate_prog_with_subst(prog: &Prog, ix: usize) -> Option<Prog> {
    match prog.get(ix).unwrap_or_else(|| panic!("Error substituting instruction")) {
        &Instr{op: Op::NOP(x), ..} => Some(prog_with_subst(prog, ix, Instr::new(Op::JMP(x)))),
        &Instr{op: Op::JMP(x), ..} => Some(prog_with_subst(prog, ix, Instr::new(Op::NOP(x)))),
        _ => None
    }
}

fn prog_with_subst(prog: &Prog, ix: usize, instr: Instr) -> Prog{
    prog.iter().take(ix).chain(
        [instr].iter())
        .chain(prog.iter().skip(ix + 1))
        .map(|x| x.clone())
        .collect()
}

#[cfg(test)]
mod tests {
    use super::{part1, part2, prog_with_subst};
    use crate::cpu::instr::{Instructions, Instr, Op};

    #[test]
    fn test_part1() {
        assert_eq!(1782, part1());
    }

    #[test]
    fn test_part2() {
        assert_eq!(797, part2());
    }

    #[test]
    fn test_instr_subst() {
        let prog = Instructions::from_input("acc +1\nacc +1\nacc +1\nacc +1\nacc +1\n");
        assert_eq!(vec![Instr::new(Op::ACC(1)),Instr::new(Op::ACC(1)),Instr::new(Op::ACC(1)),Instr::new(Op::ACC(1)),Instr::new(Op::ACC(1))], prog);

        let subst = prog_with_subst(&prog, 2, Instr::new(Op::JMP(12)));

        assert_eq!(vec![Instr::new(Op::ACC(1)),Instr::new(Op::ACC(1)),Instr::new(Op::JMP(12)),Instr::new(Op::ACC(1)),Instr::new(Op::ACC(1))], subst);
    }
}