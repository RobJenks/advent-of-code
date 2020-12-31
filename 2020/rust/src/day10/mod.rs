use super::common;
use itertools::Itertools;

pub fn run() {
    println!("Part 1 result: {}", part1());
}

fn part1() -> u32 {
    let deltas = calculate_full_chain_deltas(
        parse_input(common::read_file("src/day10/problem-input.txt")));

    deltas[0] * deltas[2]
}

fn calculate_full_chain_deltas(values: Vec<u32>) -> [u32; 3] {
    values.iter()
        .fold((0, [0,0,1]), |(last, acc), x| match x - last {   // Init with [0,0,1] since last adapter->device == 3
            1 => (*x, [acc[0]+1,acc[1],acc[2]]),
            2 => (*x, [acc[0],acc[1]+1,acc[2]]),
            3 => (*x, [acc[0],acc[1],acc[2]+1]),
            _ => panic!("Unexpected delta of {} from {} to {}", x-last, last, x)
        }).1
}

fn parse_input(input: String) -> Vec<u32> {
    input.lines()
        .map(|x| x.parse::<u32>().unwrap_or_else(|e| panic!("Invalid input {} ({})", x, e)))
        .sorted().collect()
}

#[cfg(test)]
mod tests {
    use crate::common;
    use crate::day10::{calculate_full_chain_deltas, parse_input};

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
}