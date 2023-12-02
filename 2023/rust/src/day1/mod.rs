use itertools::Itertools;
use super::common;

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> usize {
    sum_lines(parse_input("src/day1/problem-input.txt"))
}

fn part2() -> usize {
    12
}

fn sum_lines(lines: Vec<String>) -> usize {
    lines.iter()
        .map(line_value)
        .sum()
}

fn line_value(line: &String) -> usize {
    (line.chars().find(char::is_ascii_digit).iter().flat_map(|c| c.to_digit(10))
        .next().unwrap_or_else(|| panic!("No first digit")) * 10

    + line.chars().rfind(char::is_ascii_digit).iter().flat_map(|c| c.to_digit(10))
        .next().unwrap_or_else(|| panic!("No last digit")))

    as usize
}

fn parse_input(str: &str) -> Vec<String> {
    common::read_file(str)
        .lines()
        .map(|x| x.to_string())
        .collect()
}

#[cfg(test)]
mod tests {
    use crate::day1::{ part1, part2, parse_input, sum_lines };

    #[test]
    fn test_part1() {
        assert_eq!(sum_lines(parse_input("src/day1/test-input-1.txt")), 142);
    }
}