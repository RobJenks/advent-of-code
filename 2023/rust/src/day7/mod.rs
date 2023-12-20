mod cards;

use std::iter::Iterator;
use itertools::Itertools;
use crate::day7::cards::{Card, Hand, Round};
use super::common;

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> usize {
    get_total_winnings(parse_input("src/day7/problem-input.txt"), false)
}

fn part2() -> usize {
    get_total_winnings(parse_input("src/day7/problem-input.txt"), true)
}

fn parse_input(file: &str) -> Vec<Round> {
    common::read_file(file)
        .lines()
        .map(|x| x.split_once(' ').unwrap_or_else(|| panic!("Invalid input format")))
        .map(|x| Round::new(parse_hand(x.0), x.1.parse::<usize>().unwrap_or_else(|_| panic!("Invalid bid"))))
        .collect_vec()
}

fn parse_hand(str: &str) -> Hand {
    Hand::calculate(str.chars().collect_vec())
}

fn sort_by_rank(rounds: &Vec<Round>) -> Vec<&Round> {
    rounds.iter().sorted_by(|&r0, &r1| r0.hand.cmp(&r1.hand)).collect_vec()
}

fn get_total_winnings(all_rounds: Vec<Round>, allow_optimization: bool) -> usize {
    let rounds = if allow_optimization { optimize_rounds(&all_rounds) } else { all_rounds };
    sort_by_rank(&rounds)
        .iter().enumerate()
        .map(|(rank, &round)| (rank + 1) * round.bid)
        .sum()
}

fn optimize_rounds(rounds: &Vec<Round>) -> Vec<Round> {
    let joker_alts = joker_alternatives();
    rounds.iter().map(|r| Round::new(optimize_hand(&r.hand, &joker_alts), r.bid)).collect_vec()
}

fn optimize_hand(current_hand: &Hand, joker_alts: &Vec<Card>) -> Hand {
    let hand = Hand::new_from_cards(&current_hand.cards.iter()
        .map(|c| if c.name == 'J' { Card::new('J', 0) } else { c.clone() })
        .collect_vec());

    let singles = hand.cards.iter().map(|c| vec![c.clone()]).collect_vec();
    let options = hand.cards.iter().enumerate()
        .map(|(i, c)| if c.name == 'J' { &joker_alts } else { &singles[i] })
        .collect_vec();

    let mut best_type = hand.hand_type;
    for c0 in options[0] {
        for c1 in options[1] {
            for c2 in options[2] {
                for c3 in options[3] {
                    for c4 in options[4] {
                        let new_hand = Hand::calculate(vec![c0.name, c1.name, c2.name, c3.name, c4.name]);
                        if new_hand.hand_type > best_type {
                            best_type = new_hand.hand_type;
                        }
                    }
                }
            }
        }
    }

    Hand { cards: hand.cards, hand_type: best_type }
}

fn joker_alternatives() -> Vec<Card> {
    vec![
        Card { name: '2', value: 0 },
        Card { name: '3', value: 0 },
        Card { name: '4', value: 0 },
        Card { name: '5', value: 0 },
        Card { name: '6', value: 0 },
        Card { name: '7', value: 0 },
        Card { name: '8', value: 0 },
        Card { name: '9', value: 0 },
        Card { name: 'T', value: 0 },
        Card { name: 'Q', value: 0 },
        Card { name: 'K', value: 0 },
        Card { name: 'A', value: 0 }
    ]
}

#[cfg(test)]
mod tests {
    use crate::day7::{get_total_winnings, joker_alternatives, optimize_hand, parse_hand, parse_input, part1, part2};
    use crate::day7::cards::*;

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
        assert_eq!(get_total_winnings(parse_input("src/day7/test-input-1.txt"), false), 6440);
    }

    #[test]
    fn test_optimize_hand() {
        assert_eq!(parse_hand("T55J5").hand_type.name, HAND_THREE_OF_A_KIND.name);
        assert_eq!(optimize_hand(&parse_hand("T55J5"), &joker_alternatives()).hand_type.name, HAND_FOUR_OF_A_KIND.name);

        assert_eq!(parse_hand("KTJJT").hand_type.name, HAND_TWO_PAIR.name);
        assert_eq!(optimize_hand(&parse_hand("KTJJT"), &joker_alternatives()).hand_type.name, HAND_FOUR_OF_A_KIND.name);
    }

    #[test]
    fn test_optimized_hand_rank() {
        assert_eq!(get_total_winnings(parse_input("src/day7/test-input-1.txt"), true), 5905);
    }

    #[test]
    fn test_part1() {
        assert_eq!(part1(), 247823654);
    }
    #[test]
    fn test_part2() {
        assert_eq!(part2(), 245461700);
    }

}
