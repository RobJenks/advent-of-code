use std::iter::Iterator;
use std::ops::Rem;
use itertools::Itertools;
use super::common;

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> usize {
    hash_sequence(&parse_input("src/day15/problem-input.txt"))
}

fn part2() -> usize {
    12
}

fn hash(str: &str) -> u32 {
    str.chars().fold(0u32, |acc, c| ((acc + (c as u32)) * 17).rem(256))
}

fn hash_sequence(seq: &Vec<String>) -> usize {
    seq.iter()
        .map(|s| hash(s) as usize)
        .sum::<usize>()
}

fn parse_input(file: &str) -> Vec<String> {
    common::read_file(file)
        .split(',')
        .map(|x| x.trim().to_string())
        .collect_vec()
}

#[cfg(test)]
mod tests {
    use crate::day15::{hash, hash_sequence, parse_input, part1, part2};

    #[test]
    fn test_hash() {
        assert_eq!(hash("HASH"), 52);
    }

    #[test]
    fn test_hash_sequence() {
        assert_eq!(hash_sequence(&parse_input("src/day15/test-input-1.txt")), 1320);
    }

    #[test]
    fn test_part1() {
        assert_eq!(part1(), 506869);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(), 12);
    }

}
