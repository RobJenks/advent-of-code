mod cards;

use std::iter::Iterator;
use itertools::Itertools;
use crate::day7::cards::{Hand, Round};
use super::common;

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> usize {
    get_total_winnings(&parse_input("src/day7/problem-input.txt"))
}

fn part2() -> usize {
    12
}

fn parse_input(file: &str) -> Vec<Round> {
    common::read_file(file)
        .lines()
        .map(|x| x.split_once(' ').unwrap_or_else(|| panic!("Invalid input format")))
        .map(|x| Round::new(parse_hand(x.0), x.1.parse::<usize>().unwrap_or_else(|_| panic!("Invalid bid"))))
        .collect_vec()
}

fn parse_hand(str: &str) -> Hand {
    let mut hand = Hand::new(str.chars().collect_vec());
    hand.hand_type = Hand::get_hand_type(&hand);
    hand
}

fn sort_by_rank(rounds: &Vec<Round>) -> Vec<&Round> {
    rounds.iter().sorted_by(|&r0, &r1| r0.hand.cmp(&r1.hand)).collect_vec()
}

fn get_total_winnings(rounds: &Vec<Round>) -> usize {
    sort_by_rank(rounds)
        .iter().enumerate()
        .map(|(rank, &round)| (rank + 1) * round.bid)
        .sum()
}

#[cfg(test)]
mod tests {
    use crate::day7::{get_total_winnings, parse_hand, parse_input, part1, part2 };
    use crate::day7::cards::{Hand, HAND_ONE_PAIR, HAND_THREE_OF_A_KIND, HAND_TWO_PAIR };

    #[test]
    fn test_hand_type() {
        assert_eq!(Hand::get_hand_type(&parse_hand("32T3K")), HAND_ONE_PAIR);
        assert_eq!(Hand::get_hand_type(&parse_hand("T55J5")), HAND_THREE_OF_A_KIND);
        assert_eq!(Hand::get_hand_type(&parse_hand("KK677")), HAND_TWO_PAIR);
        assert_eq!(Hand::get_hand_type(&parse_hand("KTJJT")), HAND_TWO_PAIR);
        assert_eq!(Hand::get_hand_type(&parse_hand("QQQJA")), HAND_THREE_OF_A_KIND);
    }

    #[test]
    fn test_hand_rank() {
        assert_eq!(get_total_winnings(&parse_input("src/day7/test-input-1.txt")), 6440);
    }

    #[test]
    fn test_part1() {
        assert_eq!(part1(), 247823654);
    }
    #[test]
    fn test_part2() {
        assert_eq!(part2(), 12);
    }

}
