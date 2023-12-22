use std::collections::HashSet;
use std::iter::Iterator;
use itertools::Itertools;
use crate::common::grid::Grid;
use crate::common::vec::Vec2;
use super::common;

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> usize {
    calculate_north_load(&get_north_tilted_state(&parse_input("src/day14/problem-input.txt")))
}

fn part2() -> usize {
    calculate_north_load(
        &get_state_after_cycles(
            &parse_input("src/day14/problem-input.txt"), 1000000000))
}

fn get_state_after_cycles(input: &Grid<char>, cycles: usize) -> Grid<char> {
    let mut grid = input.clone();

    let mut seen = HashSet::<String>::new();
    seen.insert(dump_grid(&grid));

    let mut cycle_start = 0usize;
    let mut ix = 0usize;
    while ix < cycles {
        cycle(&mut grid);

        let new_key = dump_grid(&grid);
        if seen.contains(&new_key) {
            if cycle_start == 0 {
                cycle_start = ix;
                seen.clear();
            }
            else {
                let cycle_length = ix - cycle_start;
                let remaining_cycles = (cycles - ix) / cycle_length;

                ix += remaining_cycles * cycle_length;
            }
        }

        seen.insert(new_key);
        ix += 1
    }

    grid
}

fn dump_grid(grid: &Grid<char>) -> String {
    grid.raw_data().iter().collect::<String>()
}

fn cycle(grid: &mut Grid<char>) {
    tilt_north(grid);
    tilt_west(grid);
    tilt_south(grid);
    tilt_east(grid);
}

fn get_north_tilted_state(input: &Grid<char>) -> Grid<char> {
    let mut grid = input.clone();
    tilt_north(&mut grid);
    grid
}

fn tilt_north(grid: &mut Grid<char>) {
    let size = grid.get_size();

    (0..size.x())
        .for_each(|col| {
            let mut block = -1isize;
            for row in 0..size.y() {
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
}

fn tilt_south(grid: &mut Grid<char>) {
    let size = grid.get_size();

    (0..size.x())
        .for_each(|col| {
            let mut block = size.y();
            for row in (0..size.y()).rev() {
                let ix = grid.coords_to_ix(col, row);
                let val = grid.get(ix);
                match val {
                    '#' => block = row,
                    'O' => {
                        if block != row {
                            grid.set(ix, &'.');
                            grid.set(grid.coords_to_ix(col, block - 1), &'O');
                            block -= 1;
                        }
                    },
                    _ => { }
                }
            }
        });
}

fn tilt_west(grid: &mut Grid<char>) {
    let size = grid.get_size();

    (0..size.y())
        .for_each(|row| {
            let mut block = -1isize;
            for col in 0..size.x() {
                let ix = grid.coords_to_ix(col, row);
                let val = grid.get(ix);
                match val {
                    '#' => block = col as isize,
                    'O' => {
                        if block != col as isize {
                            grid.set(ix, &'.');
                            grid.set(grid.coords_to_ix((block + 1) as usize, row), &'O');
                            block += 1;
                        }
                    },
                    _ => { }
                }
            }
        });
}

fn tilt_east(grid: &mut Grid<char>) {
    let size = grid.get_size();

    (0..size.y())
        .for_each(|row| {
            let mut block = size.x();
            for col in (0..size.x()).rev() {
                let ix = grid.coords_to_ix(col, row);
                let val = grid.get(ix);
                match val {
                    '#' => block = col,
                    'O' => {
                        if block != col {
                            grid.set(ix, &'.');
                            grid.set(grid.coords_to_ix(block - 1, row), &'O');
                            block -= 1;
                        }
                    },
                    _ => { }
                }
            }
        });
}

fn calculate_north_load(grid: &Grid<char>) -> usize {
    let rows = grid.get_size().y();
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
    use crate::day14::{parse_input, part1, part2, calculate_north_load, get_state_after_cycles, get_north_tilted_state};

    #[test]
    fn test_tilted_state() {
        assert_eq!(
            calculate_north_load(
                &get_north_tilted_state(
                    &parse_input("src/day14/test-input-1.txt"))),
            136);
    }

    #[test]
    fn test_cycle_calculation() {
        assert_eq!(
            calculate_north_load(
                &get_state_after_cycles(
                    &parse_input("src/day14/test-input-1.txt"), 1000000000)),
            64);
    }

    #[test]
    fn test_part1() {
        assert_eq!(part1(), 108955);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(), 106689);
    }

}
