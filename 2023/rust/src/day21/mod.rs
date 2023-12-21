use std::collections::HashSet;
use std::iter::Iterator;
use itertools::Itertools;
use crate::common::grid::Grid;
use super::common;

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> usize {
    Some(parse_input("src/day21/problem-input.txt"))
        .map(|(grid, start)| get_cells_reached(&grid, start, 64))
        .unwrap().len()
}

fn part2() -> usize {
    12
}


fn get_cells_reached(grid: &Grid<char>, start: usize, steps: usize) -> HashSet<usize> {
    let mut active = HashSet::new();
    active.insert(start);

    for _ in 0..steps {
        let new_active = active.iter()
            .flat_map(|&ix| [grid.get_left(ix), grid.get_up(ix), grid.get_right(ix), grid.get_down(ix)].into_iter()
                .filter(|adj| adj.is_some())
                .map(|adj| adj.unwrap())

                .filter(|adj| grid.get(*adj) == '.'))

            .collect::<HashSet<usize>>();

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

#[cfg(test)]
mod tests {
    use crate::day21::{get_cells_reached, parse_input, part1, part2};

    #[test]
    fn test_step_calculation() {
        assert_eq!(Some(parse_input("src/day21/test-input-1.txt"))
            .map(|(grid, start)| get_cells_reached(&grid, start, 6)).unwrap().len(),
                   16);
    }

    #[test]
    fn test_part1() {
        assert_eq!(part1(), 3649);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(), 12);
    }

}
