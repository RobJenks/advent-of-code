use super::common;
use itertools::Itertools;
use std::collections::HashMap;

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> usize {
    execute_to_memory(StoreMode::Direct, parse_input(common::read_file("src/day14/problem-input.txt")))
        .values()
        .sum()
}

fn part2() -> usize {
    execute_to_memory(StoreMode::Distributed, parse_input(common::read_file("src/day14/problem-input.txt")))
        .values()
        .sum()
}

fn execute_to_memory(mode: StoreMode, commands: Vec<Command>) -> HashMap<usize, usize> {
    commands.iter().fold((HashMap::new(), Mask::none()), |(mut m, mask), x| {
        match x {
            Command::Mask(mask_data) => (m, Mask::parse(mask_data, mode)),
            Command::Store(addr, x) => {
                execute_store(&mode, &mut m, &mask, *addr, *x);
                (m, mask)
            }
        }
    }).0
}

fn execute_store(mode: &StoreMode, m: &mut HashMap<usize, usize>, mask: &Mask, addr: usize, val: usize) {
    match mode {
        StoreMode::Direct => execute_store_direct(m, mask, addr, val),
        StoreMode::Distributed => execute_store_distributed(m, mask, addr, val)
    }
}

fn execute_store_direct(m: &mut HashMap<usize, usize>, mask: &Mask, addr: usize, val: usize) {
    m.insert(addr, mask.apply(val));
}

fn execute_store_distributed(m: &mut HashMap<usize, usize>, mask: &Mask, addr: usize, val: usize) {
    generate_distributed_addresses(&mask, addr).iter()
        .for_each(|x| { m.insert(*x, val); });
}

fn generate_distributed_addresses(mask: &Mask, addr: usize) -> Vec<usize> {
    let bin = format!("{:036b}", addr);
    let init_apply = mask.apply_raw_with_floating(bin.as_str());

    let mut addresses = vec![init_apply];
    while let Some(ix) = addresses.get(0).unwrap().rfind('X') {
        let mut new_addresses = Vec::new();
        addresses.iter().for_each(|m| {
            let (pre, post) = m.split_at(ix);
            new_addresses.push(format!("{}0{}", pre, &post[1..]));
            new_addresses.push(format!("{}1{}", pre, &post[1..]));
        });

        addresses = new_addresses;
    }

    addresses.iter().map(|s| Mask::value(s)).collect()
}

#[derive(Clone, Debug)]
struct Mask {
    pub mask_or: usize,             // 1 for set-1, 0 for X/set-0
    pub mask_nand: usize,           // 1 for set-0, 0 for X/set-1
    pub raw_value: Option<String>,  // Raw value
}

#[derive(Copy, Clone, Debug)]
enum StoreMode { Direct, Distributed }

impl Mask {
    fn none() -> Self {
        Self::new_bitwise(0, 0)
    }

    fn new_bitwise(mask_or: usize, mask_nand: usize) -> Self {
        Self {mask_or, mask_nand, raw_value: None }
    }

    fn new_raw(raw_value: &str) -> Self {
        Self {mask_or: 0, mask_nand: 0, raw_value: Some(raw_value.to_string()) }
    }

    pub fn parse(s: &str, mode: StoreMode) -> Self {
        match mode {
            StoreMode::Direct => Self::parse_direct(s),
            StoreMode::Distributed => Self::parse_distributed(s)
        }
    }

    fn parse_direct(s: &str) -> Self {
        s.chars().rev()
            .enumerate()
            .fold(Self::none(), |mask, (i, ch)| {
                match ch {
                    'X' => mask.clone(),
                    '0' => Mask::new_bitwise(mask.mask_or, mask.mask_nand + 2usize.pow(i as u32)),
                    '1' => Mask::new_bitwise(mask.mask_or + 2usize.pow(i as u32), mask.mask_nand),

                    _ => panic!("Unrecognised mask element: {}", ch)
                }
            })
    }

    fn parse_distributed(s: &str) -> Self {
        Mask::new_raw(s)
    }

    pub fn apply(&self, x: usize) -> usize {
        (x | self.mask_or) & !self.mask_nand
    }

    pub fn apply_raw_with_floating(&self, x: &str) -> String {
        self.raw_value.as_ref().unwrap_or_else(|| panic!("No raw value for floating bitmask substitution"))
            .chars().zip(x.chars())
            .map(|(m, b)| match m {
                '0' => b,
                '1' => '1',
                'X' => 'X',
                _ => panic!("Invalid mask value '{}' when applying raw mask '{}' to '{}'", m, self.raw_value.as_ref().unwrap(), x)
            })
            .collect::<String>()
    }

    fn value(s: &str) -> usize {
        s.chars().rev().enumerate().fold(0, |acc, (i, x)| {
            match x {
                '0' => acc,
                '1' => acc + 2usize.pow(i as u32),
                _ => panic!("Invalid mask value '{}' at index {} of '{}'", x, i, s)
            }
        })
    }
}

#[derive(Debug)]
enum Command {
    Mask(String),
    Store(usize, usize)
}

fn parse_input(input: String) -> Vec<Command> {
    input.lines()
        .map(|x| x.split(" = ").collect_tuple::<(&str, &str)>().unwrap_or_else(|| panic!("Invalid input: {}", x)))
        .map(|(op, x)| match op {
            "mask" => Command::Mask(x.to_string()),
            _ => Command::Store(store_address(op), x.parse::<usize>().unwrap_or_else(|e| panic!("Invalid store value: {} ({})", x, e)))
        })
        .collect()
}

fn store_address(op: &str) -> usize {
    op[4..].trim_end_matches(']').parse::<usize>().unwrap_or_else(|e| panic!("Invalid store op: {} ({})", op, e))
}


#[cfg(test)]
mod tests {
    use crate::day14::{Mask, part1, StoreMode, generate_distributed_addresses, part2};
    use itertools::Itertools;

    #[test]
    fn test_part1() {
        assert_eq!(9879607673316, part1());
    }

    #[test]
    fn test_part2() {
        assert_eq!(3435342392262, part2());
    }

    #[test]
    fn test_mask_application() {
        assert_eq!(73, Mask::parse("XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X", StoreMode::Direct).apply(11));
        assert_eq!(101, Mask::parse("XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X", StoreMode::Direct).apply(101));
        assert_eq!(64, Mask::parse("XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X", StoreMode::Direct).apply(0));
    }

    #[test]
    fn test_distributed_address_generation() {
        assert_eq!(vec![26, 27, 58, 59], generate_distributed_addresses(
            &Mask::new_raw("000000000000000000000000000000X1001X"), 42).iter().sorted().cloned().collect::<Vec<_>>());
    }
}