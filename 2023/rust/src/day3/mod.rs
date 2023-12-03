use std::iter::Iterator;
use crate::common::grid::Grid;
use crate::common::vec2::Vec2;
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
    12
}

fn sum_part_numbers(grid: &Grid<char>) -> usize {
    let mut sum = 0usize;
    let size = grid.get_size();

    for y in 0..size.y {
        let mut in_number = false;
        let mut start = 0usize;

        for x in 0..size.x {
            let c = grid.get_at_coords(x, y);

            if in_number {
                if !c.is_ascii_digit() || x == (size.x - 1) {   // Just reached the end of a number
                    let end = if !c.is_ascii_digit() { x - 1 } else { x };
                    let adj = grid.get_adjacent_to_region(&Vec2::new(start, y), &Vec2::new(end, y), true);

                    if adj.iter().map(|&ix| grid.get(ix))
                        .any(|a| !a.is_ascii_digit() && a != '.') {    // If any surrounding char is a symbol (not a digit, or '.') this is a part number

                        sum += get_value(grid, y, start, end);
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
    sum
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
    use super::{ parse_input, sum_part_numbers };

    #[test]
    fn test_part_adjacency() {
        assert_eq!(sum_part_numbers(&parse_input("src/day3/test-input-1.txt")), 4361);
    }
}
