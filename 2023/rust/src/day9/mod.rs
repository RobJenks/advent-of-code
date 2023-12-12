use std::iter::Iterator;
use itertools::Itertools;
use super::common;

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> isize {
    parse_input("src/day9/problem-input.txt").iter()
        .map(|hist| calculate_next_value(&generate_history(hist)))
        .sum()
}

fn part2() -> isize {
    parse_input("src/day9/problem-input.txt").iter()
        .map(|hist| calculate_first_value(&generate_history(hist)))
        .sum()
}

fn parse_input(file: &str) -> Vec<Vec<isize>> {
    common::read_file(file)
        .lines()
        .map(|line| line.split_ascii_whitespace()
            .map(|s| s.parse::<isize>().unwrap_or_else(|_| panic!("Invalid input")))
            .collect_vec())
        .collect_vec()
}

fn generate_history(input: &Vec<isize>) -> Vec<Vec<isize>> {
    let mut history = vec![input.clone()];
    while !history.last().unwrap().iter().all(|&v| v == 0) {
        history.push(generate_next_history(history.last().unwrap()))
    }

    history
}

fn generate_next_history(current: &Vec<isize>) -> Vec<isize> {
    current.iter().zip(current.iter().skip(1))
        .map(|(&a, &b)| b - a)
        .collect_vec()
}

fn calculate_next_value(history: &Vec<Vec<isize>>) -> isize {
    history.iter()
        .rev()
        .skip(1)
        .fold(0, |acc, x| x.last().unwrap() + acc)
}

fn calculate_first_value(history: &Vec<Vec<isize>>) -> isize {
    history.iter()
        .rev()
        .skip(1)
        .fold(0, |acc, x| x.first().unwrap() - acc)
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;
    use crate::day9::{calculate_first_value, calculate_next_value, generate_history, parse_input, part1, part2};

    #[test]
    fn test_history_generation() {
        assert_eq!(parse_input("src/day9/test-input-1.txt").iter()
            .map(|hist| calculate_next_value(&generate_history(hist)))
            .collect_vec(),

            vec![18, 28, 68]);
    }

    #[test]
    fn test_previous_value_generation() {
        assert_eq!(parse_input("src/day9/test-input-1.txt").iter()
            .map(|hist| calculate_first_value(&generate_history(hist)))
            .collect_vec(),

            vec![-3, 0, 5]);
    }

    #[test]
    fn test_part1() {
        assert_eq!(part1(), 1884768153);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(), 12);
    }

}
