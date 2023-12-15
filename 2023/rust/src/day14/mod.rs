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
    calculate_north_load(&get_tilted_state(&parse_input("src/day14/problem-input.txt")))
}

fn part2() -> usize {
    12
}

fn get_tilted_state(input: &Grid<char>) -> Grid<char> {
    let mut grid = input.clone();
    let size = input.get_size();

    (0..size.x)
        .for_each(|col| {
            let mut block = -1isize;
            for row in 0..size.y {
                let ix = grid.coords_to_ix(col, row);
                let val = grid.get(ix);
                match val {
                    '#' => block = row as isize,
                    'O' => {
                        if block != row as isize {
                            grid.set(ix, &'.');
                            grid.set(grid.coords_to_ix(col, (block + 1) as usize), &'O');
                            block += 1;
                        }
                    },
                    _ => { }
                }
            }
        });

    grid
}

fn calculate_north_load(grid: &Grid<char>) -> usize {
    let rows = grid.get_size().y;
    grid.raw_data().iter().enumerate()
        .filter(|&(_, c)| *c == 'O')
        .map(|(ix, _)| rows - grid.get_row_index(ix))
        .sum()
}

fn parse_input(file: &str) -> Grid<char> {
    let data = common::read_file(file).lines().map(|s| s.trim().to_string()).collect_vec();
    Grid::new_with_owned_data(Vec2::new(data.iter().next().unwrap().len(), data.len()),
        data.iter()
            .flat_map(|s| s.trim().chars())
            .collect_vec())
}

#[cfg(test)]
mod tests {
    use crate::day14::{parse_input, get_tilted_state, part1, part2, calculate_north_load};

    #[test]
    fn test_tilted_state() {
        assert_eq!(calculate_north_load(&get_tilted_state(&parse_input("src/day14/test-input-1.txt"))), 136);
    }

    #[test]
    fn test_part1() {
        assert_eq!(part1(), 12);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(), 12);
    }

}
