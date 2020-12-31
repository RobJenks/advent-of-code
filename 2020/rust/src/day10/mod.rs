use super::common;
use itertools::Itertools;

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> u32 {
    let deltas = calculate_full_chain_deltas(
        parse_input(common::read_file("src/day10/problem-input.txt")));

    deltas[0] * deltas[2]
}

fn part2() -> u64 {
    calculate_valid_combinations(
        &parse_input(common::read_file("src/day10/problem-input.txt")))
}

fn calculate_full_chain_deltas(values: Vec<u32>) -> [u32; 3] {
    values.iter()
        .skip(1)    // Skip first element since this is the base 0 value
        .fold((0, [0,0,0]), |(last, acc), x| match x - last {
            1 => (*x, [acc[0]+1,acc[1],acc[2]]),
            2 => (*x, [acc[0],acc[1]+1,acc[2]]),
            3 => (*x, [acc[0],acc[1],acc[2]+1]),
            _ => panic!("Unexpected delta of {} from {} to {}", x-last, last, x)
        }).1
}

fn calculate_valid_combinations(values: &Vec<u32>) -> u64 {
    let mut combinations = InfVec::new(vec![0; (*values.iter().max().unwrap()+3) as usize], 0isize);
    combinations.set(0isize, 1isize);

    values.iter()
        .skip(1)
        .map(|v| *v as isize)
        .for_each(|v| combinations.set(v, combinations.get(v-3) + combinations.get(v-2) + combinations.get(v-1)));

    combinations.get(*values.iter().rev().skip(1).next().unwrap() as isize) as u64
}

struct InfVec {
    data: Vec<isize>,
    default: isize
}

impl InfVec {
    pub fn new<T>(data: Vec<T>, default: T) -> Self
    where T: Into<isize> {
        Self {
            data: data.into_iter().map(|x| x.into()).collect(),
            default: default.into()
        }
    }

    pub fn get(&self, ix: isize) -> isize {
        if ix < 0 { self.default } else {
            *self.data.get(ix as usize).unwrap_or_else(|| &self.default)
        }
    }

    pub fn get_mut(&mut self, ix: isize) -> &mut isize {
        if ix < 0 { panic!("Cannot write out of bounds (ix: {})", ix) } else {
            self.data.get_mut(ix as usize).unwrap_or_else(|| panic!("Cannot write out of bounds (ix: {})", ix))
        }
    }

    pub fn set<T>(&mut self, ix: isize, val: T)
    where T: Into<isize> {
        *self.get_mut(ix) = val.into()
    }
}


fn parse_input(input: String) -> Vec<u32> {
    let mut values = input.lines()
        .map(|x| x.parse::<u32>().unwrap_or_else(|e| panic!("Invalid input {} ({})", x, e)))
        .sorted().collect::<Vec<_>>();

    values.insert(0, 0);                     // Insert base value
    values.push(*values.iter().max().unwrap() + 3);  // Insert device value
    values
}

#[cfg(test)]
mod tests {
    use crate::common;
    use crate::day10::{calculate_full_chain_deltas, parse_input, part1, calculate_valid_combinations, part2};

    #[test]
    fn test_part1() {
        assert_eq!(1836, part1());
    }

    #[test]
    fn test_part2() {
        assert_eq!(43406276662336, part2());
    }

    #[test]
    fn test_full_chain_deltas_1() {
        assert_eq!([7, 0, 5], calculate_full_chain_deltas(
            parse_input(common::read_file("src/day10/test-input-01.txt"))));
    }

    #[test]
    fn test_full_chain_deltas_2() {
        assert_eq!([22,0,10], calculate_full_chain_deltas(
            parse_input(common::read_file("src/day10/test-input-02.txt"))));
    }

    #[test]
    fn test_valid_combinations_1() {
        assert_eq!(8, calculate_valid_combinations(
            &parse_input(common::read_file("src/day10/test-input-01.txt"))));
    }

    #[test]
    fn test_valid_combinations_2() {
        assert_eq!(19208, calculate_valid_combinations(
            &parse_input(common::read_file("src/day10/test-input-02.txt"))));
    }

    #[test]
    fn test_valid_combinations_interfering_break() {
        assert_eq!(9, calculate_valid_combinations(&parse_input("1\n4\n5\n6\n8\n10\n11\n12\n15\n16\n19".to_string())));
    }

}