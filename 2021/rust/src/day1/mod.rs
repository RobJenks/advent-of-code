use itertools::Itertools;
use super::common;

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> usize {
    count_inc(&parse_input(
        common::read_file("src/day1/problem-input.txt").as_str()))
}

fn part2() -> usize {
    sliding_window(&parse_input(
        common::read_file("src/day1/problem-input.txt").as_str()))
}

fn count_inc(values: &Vec<u32>) -> usize {
    values.windows(2)
        .map(|w| w.iter().collect_tuple::<(&u32, &u32)>().expect("Invalid input"))
        .filter(|(&x, &y)| y > x)
        .count()
}

fn sliding_window(values: &Vec<u32>) -> usize {
    values.windows(3)
        .map(|w| w.iter().sum())
        .fold((0, None), |(acc, last), el: u32| match last {
            None => (acc, Some(el)),
            Some(prev) if el > prev => (acc + 1, Some(el)),
            Some(_) => (acc, Some(el))
        }).0
}

fn parse_input(str: &str) -> Vec<u32> {
    str.lines()
        .map(|s| s.parse::<u32>().expect("Invalid input"))
        .collect()
}

#[cfg(test)]
mod tests {
    use crate::day1::{ part1, part2, parse_input, count_inc, sliding_window };

    #[test]
    fn test_part1() {
        assert_eq!(part1(), 1791);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(), 1822);
    }

    #[test]
    fn test_count_inc() {
        assert_eq!(count_inc(&parse_input("199\n200\n208\n210\n200\n207\n240\n269\n260\n263")), 7);
    }

    #[test]
    fn test_sliding_window() {
        assert_eq!(sliding_window(&parse_input("199\n200\n208\n210\n200\n207\n240\n269\n260\n263")), 5);
    }
}