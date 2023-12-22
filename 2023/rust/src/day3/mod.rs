use std::collections::HashSet;
use std::iter::Iterator;
use crate::common::grid::Grid;
use crate::common::vec::Vec2;
use super::common;

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> usize {
    sum_part_numbers(
        &parse_input("src/day3/problem-input.txt"))
}

fn part2() -> usize {
    sum_gear_ratios(
        &parse_input("src/day3/problem-input.txt"))
}

#[derive(Eq, PartialEq, Hash, Clone)]
struct PartNumber {
    pub value : usize,
    pub row : usize,
    pub col_start : usize,
    pub col_end : usize
}

impl PartNumber {
    pub fn new(value: usize, row: usize, col_start: usize, col_end: usize) -> Self {
        Self { value, row, col_start, col_end }
    }
    pub fn get_value(&self) -> usize { self.value }
}

fn sum_part_numbers(grid: &Grid<char>) -> usize {
    find_part_numbers(grid).iter()
        .map(PartNumber::get_value)
        .sum()
}

fn sum_gear_ratios(grid: &Grid<char>) -> usize {
    let parts = find_part_numbers(grid);

    grid.raw_data().iter().enumerate()
        .filter(|&(_, &c)| c == '*')
        .map(|(ix, _)| get_parts_adjacent_to(ix, grid, &parts))
        .filter(|adj| adj.len()  == 2)
        .map(|adj| adj.iter().map(PartNumber::get_value).product::<usize>())
        .sum()
}

fn get_parts_adjacent_to(ix: usize, grid: &Grid<char>, parts: &Vec<PartNumber>) -> HashSet<PartNumber> {
    let coord = grid.ix_to_coord(ix);
    let adj = grid.get_adjacent_to_region(&coord, &coord, true)
        .iter().map(|&i| grid.ix_to_coord(i)).collect::<Vec<_>>();

    adj.iter()
        .flat_map(|coord| get_parts_overlapping_coord(parts, coord).clone())
        .collect::<HashSet<PartNumber>>()
}

fn get_parts_overlapping_coord(parts: &Vec<PartNumber>, coord: &Vec2<usize>) -> Vec<PartNumber> {
    parts.iter()
        .filter(|&part| (coord.y() == part.row) && (coord.x() >= part.col_start && coord.x() <= part.col_end))
        .cloned()
        .collect()
}

fn find_part_numbers(grid: &Grid<char>) -> Vec<PartNumber> {
    let mut result = Vec::new();
    let size = grid.get_size();

    for y in 0..size.y() {
        let mut in_number = false;
        let mut start = 0usize;

        for x in 0..size.x() {
            let c = grid.get_at_coords(x, y);

            if in_number {
                if !c.is_ascii_digit() || x == (size.x() - 1) {   // Just reached the end of a number
                    let end = if !c.is_ascii_digit() { x - 1 } else { x };
                    let adj = grid.get_adjacent_to_region(&Vec2::new(start, y), &Vec2::new(end, y), true);

                    if adj.iter().map(|&ix| grid.get(ix))
                        .any(|a| !a.is_ascii_digit() && a != '.') {    // If any surrounding char is a symbol (not a digit, or '.') this is a part number

                        result.push(PartNumber::new(get_value(grid, y, start, end), y, start, end));
                    }
                    in_number = false;
                }
            }
            else {
                if c.is_ascii_digit() {                         // Just reached the start of a number
                    in_number = true;
                    start = x;
                }
            }

        }
    }
    result
}

fn get_value(grid: &Grid<char>, row: usize, col_start: usize, col_end: usize) -> usize {
    (col_start..=col_end)
        .map(|col| grid.get_at_coords(col, row))
        .collect::<String>()
        .parse::<usize>()
        .unwrap_or_else(|e| panic!("Value on row {}, columns {} to {} is not numeric ({})", row, col_start, col_end, e))
}

fn parse_input(file: &str) -> Grid<char> {
    let lines = common::read_file(file)
        .lines()
        .map(str::to_string)
        .collect::<Vec<_>>();

    let mut grid = Grid::<char>::new(Vec2::new(
        lines.iter().next().unwrap_or_else(|| panic!("No data")).len(),
        lines.len()), &' ');

    for (y, line) in lines.iter().enumerate() {
        for (x, c) in line.chars().enumerate() {
            grid.set_at_coords(x, y, &c);
        }
    }

    grid
}


#[cfg(test)]
mod tests {
    use super::{part1, part2, parse_input, sum_part_numbers, sum_gear_ratios};

    #[test]
    fn test_part1() {
        assert_eq!(part1(), 553825);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(), 93994191);
    }

    #[test]
    fn test_part_adjacency() {
        assert_eq!(sum_part_numbers(&parse_input("src/day3/test-input-1.txt")), 4361);
    }

    #[test]
    fn test_gear_ratios() {
        assert_eq!(sum_gear_ratios(&parse_input("src/day3/test-input-1.txt")), 467835);
    }
}
