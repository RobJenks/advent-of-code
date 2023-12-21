use std::fmt::{Display, Formatter};
use std::iter::Iterator;
use itertools::Itertools;
use crate::common::grid::Grid;
use crate::common::vec2::Vec2;
use super::common;

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> usize {
    12
}

fn part2() -> usize {
    12
}

fn get_cells_reached(input: &Grid<char>, start: usize, steps: usize) -> Vec<usize> {
    let mut grid = input.clone();

    let mut active = vec![start];
    for step in 0..steps {
        let new_active = active.iter()
            .flat_map(|&ix| &[grid.get_left(ix), grid.get_up(ix), grid.get_right(ix), grid.get_down(ix)].iter()
                .flat_map(|adj| adj)
                .filter(|&adj| grid.get(ix) == '.').cloned().collect_vec())
            .cloned().collect_vec();

        active = new_active;
    }

    active
}

fn parse_input(file: &str) -> (Grid<char>, usize) {
    let mut grid = Grid::new_from_2d_data(
        &common::read_file(file).lines()
            .map(|line| line.trim().chars().collect_vec())
            .collect_vec());

    let start = grid.raw_data().iter().enumerate()
        .find(|&(_, c)| *c == 'S')
        .map(|(ix, _)| ix)
        .unwrap_or_else(|| panic!("No start location"));

    grid.set(start, &'.');

    (grid, start)
}

#[derive(Clone, Copy, Eq, PartialEq)]
enum Cell {
    Garden = 0,
    Rock = 1
}
impl Display for Cell {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", if *self == Cell::Rock { '#' } else { '.' })
    }
}

#[cfg(test)]
mod tests {
    use crate::day21::{part1, part2};

    #[test]
    fn test_part1() {
        assert_eq!(part1(), 12);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(), 12);
    }

}
