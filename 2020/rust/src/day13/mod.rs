use super::common;
use itertools::Itertools;

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> usize {
    get_best_departure(&parse_input(common::read_file("src/day13/problem-input.txt")))
        .map(|x| x.bus * x.delay)
        .unwrap_or_else(|| panic!("No result"))
}

fn part2() -> usize {
    calculate_best_sequence_start(&parse_input(common::read_file("src/day13/problem-input.txt")))
}

fn get_best_departure(input: &Input) -> Option<Connection> {
    input.departures.iter()
        .filter_map(|&x| x)
        .map(|x| Connection { bus: x, delay: x - (input.depart_time % x) })
        .min_by(|x, y| x.delay.cmp(&y.delay))
}

fn calculate_best_sequence_start(input: &Input) -> usize {
    let indexed = input.departures.iter()
        .enumerate()
        .filter(|(_, x)| x.is_some())
        .map(|(i, x)| (i, x.unwrap()))
        .sorted_by(|x, y| x.0.partial_cmp(&y.0).unwrap())
        .collect::<Vec<_>>();

    // Chinese Sieve computation: https://en.wikipedia.org/wiki/Chinese_remainder_theorem#Computation
    indexed.iter().skip(1).fold((0usize, indexed[0].1), |(ts, inc), (i, x)| {
        let mut new_ts = ts;
        while (new_ts + i) % x != 0 { new_ts += inc; }
        (new_ts, inc * x)
    }).0
}


#[derive(Debug)]
struct Input {
    pub depart_time: usize,
    pub departures: Vec<Option<usize>>
}

struct Connection {
    pub bus: usize,
    pub delay: usize
}

fn parse_input(input: String) -> Input {
    let depart_time = input.lines().nth(0)
        .map(|s| s.parse::<usize>().unwrap_or_else(|e| panic!("Failed to parse target: {}", e)))
        .unwrap_or_else(|| panic!("Cannot read departure time target"));

    let departures = input.lines().nth(1)
        .map(|s| s.split(',')
            .map(|x| x.parse::<usize>().map_or_else(|_| None, |n| Some(n)))
            .collect::<Vec<_>>())
        .unwrap_or_else(|| panic!("Cannot read departures"));

    Input { depart_time, departures }
}

#[cfg(test)]
mod tests {
    use crate::day13::{calculate_best_sequence_start, parse_input, part1, part2};

    #[test]
    fn test_part1() {
        assert_eq!(5257, part1());
    }

    #[test]
    fn test_part2() {
        assert_eq!(538703333547789, part2());
    }

    #[test]
    fn test_chinese_sieve_1() {
        assert_eq!(3417, calculate_best_sequence_start(&parse_input("0\n17,x,13,19".to_string())));
    }

    #[test]
    fn test_chinese_sieve_2() {
        assert_eq!(1068781, calculate_best_sequence_start(&parse_input("0\n7,13,x,x,59,x,31,19".to_string())));
    }
}