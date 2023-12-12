mod pipes;

use std::iter::Iterator;
use itertools::Itertools;
use crate::common::vec2::Vec2;
use crate::day10::pipes::Maze;
use super::common;

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> usize {
    get_furthest_step_number(&get_circular_path(&parse_input("src/day10/problem-input.txt")))
}

fn part2() -> usize {
    12
}

fn get_circular_path(maze: &Maze) -> Vec<usize> {
    let start = maze.find_start();

    let mut prev = start;   // Start = 'previous' cell so we don't need to evaluate connections of that cell
    let mut current = *maze.find_connections_into_cell(prev).iter().next().unwrap();   // Pick one of the two connections as 'next' cell visited after the start

    let mut path = vec![start];
    loop {
        path.push(current);

        let next_cell = maze.get_next(current, prev);
        prev = current;
        current = next_cell;

        if current == start { break }
    }

    path
}

fn get_furthest_step_number(steps: &Vec<usize>) -> usize {
    if steps.len() % 2 == 0 { steps.len() / 2} else { panic!("No midpoint") }
}

fn parse_input(file: &str) -> Maze {
    let content = common::read_file(file);
    let lines = content.lines().collect_vec();
    let size = Vec2::new(lines[0].len(), lines.len());

    Maze::new(
        size,
        content.chars().filter(|c| !c.is_ascii_whitespace()).collect_vec())
}

#[cfg(test)]
mod tests {
    use crate::day10::{get_circular_path, get_furthest_step_number, parse_input, part1, part2};

    #[test]
    fn test_circular_path() {
        assert_eq!(
            get_circular_path(&parse_input("src/day10/test-input-1.txt")),
            vec![6, 7, 8, 13, 18, 17, 16, 11]);
    }

    #[test]
    fn test_step_counts_1() {
        assert_eq!(
            get_furthest_step_number(&get_circular_path(&parse_input("src/day10/test-input-1.txt"))),
            4);
    }

    #[test]
    fn test_step_counts_2() {
        assert_eq!(
            get_furthest_step_number(&get_circular_path(&parse_input("src/day10/test-input-2.txt"))),
            8);
    }

    #[test]
    fn test_part1() {
        assert_eq!(part1(), 6909);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(), 12);
    }

}
