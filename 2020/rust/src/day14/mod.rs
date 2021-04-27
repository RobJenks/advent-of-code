mod processor;

use super::common;
use itertools::Itertools;
use std::collections::HashMap;

pub fn run() {
    println!("Part 1 result: {}", part1());
}

fn part1() -> usize {
    execute_to_memory(parse_input(common::read_file("src/day14/problem-input.txt")))
        .values()
        .sum()
}

fn execute_to_memory(commands: Vec<Command>) -> HashMap<usize, usize> {
    commands.iter().fold((Mask::none(), HashMap::new()), |(mut mask, mut m), x| {
        match x {
            Command::Mask(new_mask) => (new_mask.clone(), m),
            Command::Store(addr, x) => {
                m.insert(*addr, mask.apply(*x));
                (mask, m)
            }
        }
    }).1
}

#[derive(Clone, Debug)]
struct Mask {
    pub mask_or: usize,      // 1 for set-1, 0 for X/set-0
    pub mask_nand: usize     // 1 for set-0, 0 for X/set-1
}

impl Mask {
    fn none() -> Self {
        Self::new(0, 0)
    }

    fn new(mask_or: usize, mask_and: usize) -> Self {
        Self { mask_or, mask_nand: mask_and }
    }

    pub fn parse(s: &str) -> Self {
        s.chars().rev()
            .enumerate()
            .fold(Self::none(), |mask, (i, ch)| {
                match ch {
                    'X' => mask.clone(),
                    '0' => Mask::new(mask.mask_or, mask.mask_nand + 2usize.pow(i as u32)),
                    '1' => Mask::new(mask.mask_or + 2usize.pow(i as u32), mask.mask_nand),

                    _ => panic!("Unrecognised mask element: {}", ch)
                }
            })
    }

    pub fn apply(&self, x: usize) -> usize {
        (x | self.mask_or) & !self.mask_nand
    }
}

#[derive(Debug)]
enum Command {
    Mask(Mask),
    Store(usize, usize)
}

fn parse_input(input: String) -> Vec<Command> {
    input.lines()
        .map(|x| x.split(" = ").collect_tuple::<(&str, &str)>().unwrap_or_else(|| panic!("Invalid input: {}", x)))
        .map(|(op, x)| match op {
            "mask" => Command::Mask(Mask::parse(x)),
            _ => Command::Store(store_address(op), x.parse::<usize>().unwrap_or_else(|e| panic!("Invalid store value: {} ({})", x, e)))
        })
        .collect()
}

fn store_address(op: &str) -> usize {
    op[4..].trim_end_matches(']').parse::<usize>().unwrap_or_else(|e| panic!("Invalid store op: {} ({})", op, e))
}


#[cfg(test)]
mod tests {
    use crate::day14::{Mask, part1};

    #[test]
    fn test_part1() {
        assert_eq!(9879607673316, part1());
    }

    #[test]
    fn test_mask_application() {
        assert_eq!(73, Mask::parse("XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X").apply(11));
        assert_eq!(101, Mask::parse("XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X").apply(101));
        assert_eq!(64, Mask::parse("XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X").apply(0));
    }

}