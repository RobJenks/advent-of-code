use super::common;
use itertools::Itertools;

pub fn run() {
    println!("Part 1 result: {}", part1());
}

fn part1() -> usize {
    parse_input(common::read_file("src/day6/problem-input.txt"))
        .iter()
        .map(|v| v.iter().join(""))
        .map(|s| s.chars().unique().count())
        .sum()
}

fn parse_input(input: String) -> Vec<Vec<String>> {
    input.lines().collect::<Vec<_>>()
        .split(|&s| s.trim().is_empty())
        .map(|x| x.iter().map(|&s| s.to_string()).collect::<Vec<String>>())
        .collect::<Vec<_>>()
}


#[cfg(test)]
mod tests {
    use super::{ part1 };

    #[test]
    fn test_part1() {
        assert_eq!(6549, part1());
    }

}