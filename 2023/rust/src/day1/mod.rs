use std::collections::HashMap;
use std::iter::Iterator;
use itertools::Itertools;
use regex::Regex;
use once_cell::unsync::Lazy;
use super::common;

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> usize {
    sum_lines(parse_input("src/day1/problem-input.txt"), line_value)
}

fn part2() -> usize {
    sum_lines(parse_input("src/day1/problem-input.txt"), line_value_extended)
}

fn sum_lines(lines: Vec<String>, eval: fn(&String) -> usize) -> usize {
    lines.iter()
        .map(eval)
        .sum()
}

fn line_value(line: &String) -> usize {
    (line.chars().find(char::is_ascii_digit).iter().flat_map(|c| c.to_digit(10))
        .next().unwrap_or_else(|| panic!("No first digit")) * 10

    + line.chars().rfind(char::is_ascii_digit).iter().flat_map(|c| c.to_digit(10))
        .next().unwrap_or_else(|| panic!("No last digit")))

    as usize
}

const DIGITS : Lazy<HashMap<&str, usize>> = Lazy::new(|| [("one", 1usize), ("two", 2), ("three", 3), ("four", 4), ("five", 5),
                                                          ("six", 6), ("seven", 7), ("eight", 8), ("nine", 9)].iter().cloned().collect());
const REV_DIGITS : Lazy<HashMap<String, usize>> = Lazy::new(||
    DIGITS.iter().map(|(&k, &v)| (k.chars().rev().collect::<String>(), v)).map(|x| x).collect());

const RE_FWD : Lazy<Regex> = Lazy::new(|| Regex::new(&format!("[0-9]|{}", DIGITS.keys().join("|"))).unwrap_or_else(|_| panic!("Invalid pattern")));
const RE_REV : Lazy<Regex> = Lazy::new(|| Regex::new(&format!("[0-9]|{}", REV_DIGITS.keys().join("|"))).unwrap_or_else(|_| panic!("Invalid pattern")));

fn line_value_extended(line: &String) -> usize {
    RE_FWD.find(line).iter().next()
        .map(|m| m.as_str())
        .map(|x| DIGITS.get(x).cloned().unwrap_or_else(|| x.parse::<usize>().unwrap()))
        .unwrap_or_else(|| panic!("No first value for '{}'", line)) * 10
    +
    RE_REV.find(line.chars().rev().collect::<String>().as_str()).iter().next()
        .map(|m| m.as_str())
        .map(|x| REV_DIGITS.get(x).cloned().unwrap_or_else(|| x.parse::<usize>().unwrap()))
        .unwrap_or_else(|| panic!("No last value for '{}'", line))
}

fn parse_input(file: &str) -> Vec<String> {
    common::read_file(file)
        .lines()
        .map(|x| x.to_string())
        .collect()
}

#[cfg(test)]
mod tests {
    use super::{ parse_input, sum_lines, line_value, line_value_extended };

    #[test]
    fn test_part1() {
        assert_eq!(sum_lines(parse_input("src/day1/test-input-1.txt"), line_value), 142);
    }

    #[test]
    fn test_part2() {
        assert_eq!(sum_lines(parse_input("src/day1/test-input-2.txt"), line_value_extended), 281);
    }
}