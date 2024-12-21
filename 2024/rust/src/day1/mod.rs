use std::collections::HashMap;
use itertools::Itertools;
use super::common;

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> isize {
    sorted_diffs(&parse_input("src/day1/problem-input.txt"))
}

fn part2() -> isize {
    freq_sum(&parse_input("src/day1/problem-input.txt"))
}

fn sorted_diffs((v0, v1): &(Vec<isize>, Vec<isize>)) -> isize {
    v0.iter().sorted().zip(v1.iter().sorted())
        .map(|(a, b)| (a - b).abs())
        .sum()
}

fn freq_sum((v0, v1): &(Vec<isize>, Vec<isize>)) -> isize {
    let freq = get_frequencies(&v1);
    v0.iter()
        .map(|x| x * freq.get(x).cloned().unwrap_or_else(|| 0) as isize)
        .sum()
}

fn get_frequencies(v: &Vec<isize>) -> HashMap<isize, usize> {
    v.iter()
        .cloned()
        .counts_by(|x| x)
}

fn parse_input(file: &str) -> (Vec<isize>, Vec<isize>) {
    common::read_file(file)
        .lines()
        .map(|x| x.split_whitespace().collect_tuple::<(&str, &str)>().unwrap_or_else(|| panic!("Malformed input")))
        .map(|(a, b)| (a.parse::<isize>().unwrap(), b.parse::<isize>().unwrap()))
        .unzip()
}

#[cfg(test)]
mod tests {
    use super::{part1, part2, parse_input, sorted_diffs, freq_sum};

    #[test]
    fn test_sorted_diffs() {
        assert_eq!(sorted_diffs(&parse_input("src/day1/test-input-1.txt")), 11);
    }

    #[test]
    fn test_freq_sum() {
        assert_eq!(freq_sum(&parse_input("src/day1/test-input-1.txt")), 31);
    }
}