use std::iter::Iterator;
use itertools::Itertools;
use super::common;

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> usize {
    sum_all_card_points(&parse_input("src/day4/problem-input.txt"))
}

fn part2() -> usize {
    calculate_total_cards(&parse_input("src/day4/problem-input.txt"))
}

fn sum_all_card_points(cards: &Vec<Card>) -> usize {
    cards.iter()
        .map(calculate_card_points)
        .sum()
}

fn calculate_card_points(card: &Card) -> usize {
    match calculate_card_matches(card) as u32 {
        0 => 0,
        n => 2usize.pow(n - 1)
    }
}

fn calculate_card_matches(card: &Card) -> usize {
    card.numbers.iter()
        .filter(|&n| card.winning.contains(n))
        .count()
}

fn calculate_total_cards(cards: &Vec<Card>) -> usize {
    let matches = cards.iter().map(calculate_card_matches).collect_vec();
    let mut copies = vec![1usize; cards.len()];

    for (ix, &m) in matches.iter().enumerate() {
        ((ix + 1)..=(ix + m)).for_each(|next| copies[next] += copies[ix]);
    }

    copies.iter().sum()
}

fn parse_input(file: &str) -> Vec<Card> {
    common::read_file(file)
        .lines()
        .map(parse_card)
        .collect()
}

fn parse_card(str: &str) -> Card {
    str.split(&[':', '|'])
        .collect_tuple::<(&str, &str, &str)>()
        .map(|(id_string, win_string, num_string)| Card::new(
            id_string.split_ascii_whitespace().nth(1).unwrap_or_else(|| panic!("No ID")).parse::<usize>().unwrap_or_else(|_| panic!("Invalid ID")),
            win_string.split_ascii_whitespace().map(|s| s.parse::<u32>().unwrap_or_else(|_| panic!("Invalid winning numbers"))).collect(),
            num_string.split_ascii_whitespace().map(|s| s.parse::<u32>().unwrap_or_else(|_| panic!("Invalid numbers"))).collect()
        ))
        .unwrap_or_else(|| panic!("Failed to parse card"))
}

struct Card {
    #[allow(dead_code)]
    pub id: usize,
    pub winning: Vec<u32>,
    pub numbers: Vec<u32>
}
impl Card {
    pub fn new(id: usize, winning: Vec<u32>, numbers: Vec<u32>) -> Self {
        Self { id, winning, numbers }
    }
}


#[cfg(test)]
mod tests {
    use super::{part1, part2, parse_input, sum_all_card_points, calculate_total_cards};

    #[test]
    fn test_points_calculation() {
        assert_eq!(sum_all_card_points(&parse_input("src/day4/test-input-1.txt")), 13);
    }

    #[test]
    fn test_total_cards_calculation() {
        assert_eq!(calculate_total_cards(&parse_input("src/day4/test-input-1.txt")), 30);
    }

    #[test]
    fn test_part1() {
        assert_eq!(part1(), 24542);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(), 8736438);
    }

}
