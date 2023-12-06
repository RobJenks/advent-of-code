use std::iter::Iterator;
use std::ops::Range;
use itertools::Itertools;
use super::common;

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> usize {
    parse_input("src/day6/problem-input.txt").iter()
        .map(find_winning_time_range)
        .map(|(min, max)| max - min + 1)
        .product()
}

fn part2() -> usize {
    let result = find_winning_time_range(&Race::new( 53897698, 313109012141201));
    result.1 - result.0 + 1
}

// Returns min and max time (inclusive) which will win the race
fn find_winning_time_range(race: &Race) -> (usize, usize) {
    (
        find_first_win_in_range(race, 1..race.time),
        find_first_win_in_range(race, (1..race.time).rev())
    )
}

fn find_first_win_in_range(race: &Race, range: impl IntoIterator<Item = usize>) -> usize {
    range.into_iter()
        .filter(|&hold_time| get_distance(race.time, hold_time) > race.distance)
        .next()
        .unwrap_or_else(|| panic!("No minimum bound"))
}

fn get_distance(race_time: usize, time_held: usize) -> usize {
    (race_time - time_held) * time_held
}

fn parse_input(file: &str) -> Vec<Race> {
    common::read_file(file)
        .lines()

        // Collect each set of values
        .map(|line| line.split_once(':').unwrap_or_else(|| panic!("Invalid input")).1
            .split_ascii_whitespace()
            .map(|val| val.parse::<usize>().unwrap_or_else(|_| panic!("Non-numeric data")))
            .collect_vec())
        .collect_tuple::<(Vec<_>, Vec<_>)>()

        // Combine into races
        .map(|(times, distances)| times.iter().zip(distances.iter())
            .map(|(&time, &dist)| Race::new(time, dist))
            .collect_vec())
        .unwrap_or_else(|| panic!("Could not collect data"))
}

struct Race {
    pub time: usize,
    pub distance: usize
}

impl Race {
    pub fn new(time: usize, distance: usize) -> Self {
        Self { time, distance }
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;
    use super::{part1, part2, parse_input, find_winning_time_range, Race};

    #[test]
    fn test_find_best_time() {
        assert_eq!(find_winning_time_range(&Race::new(7, 9)), (2, 5));
    }

    #[test]
    fn test_find_best_times_across_races() {
        assert_eq!(parse_input("src/day6/test-input-1.txt").iter()
            .map(find_winning_time_range)
            .collect_vec(),

            vec![(2, 5), (4, 11), (11, 19)]);
    }

    #[test]
    fn test_best_time_long_race() {
        assert_eq!(find_winning_time_range(&Race::new(71530, 940200)), (14, 71516));
    }

    #[test]
    fn test_part1() {
        assert_eq!(part1(), 5133600);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(), 40651271);
    }

}
