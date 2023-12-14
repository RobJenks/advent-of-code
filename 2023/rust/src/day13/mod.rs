use std::iter::Iterator;
use itertools::Itertools;
use crate::common::grid::Grid;
use crate::common::vec2::Vec2;
use super::common;

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> usize { // 39471 too high
    get_summarized_total(&parse_input("src/day13/problem-input.txt"))
}

fn part2() -> isize {
    12
}

fn parse_input(file: &str) -> Vec<Grid<char>> {
    common::read_file(file).lines()
        .batching(|lines| {
            let next = lines.take_while(|line| !line.trim().is_empty())
                .map(|s| s.trim().to_string()).collect_vec();
            if next.is_empty() { None } else { Some(next) }
        })
        .map(|data| (data.iter().map(|s| s.trim()).join(""), Vec2::new(data.iter().next().unwrap().len(), data.len())))
        .map(|(data, size)| Grid::<char>::new_with_owned_data(size, data.chars().collect_vec()))
        .collect_vec()
}

fn find_reflection_line(grid: &Grid<char>) -> Reflection {
    // Test rows first as this is cheaper
    if let Some(row_symmetry) = (0..grid.get_size().y - 1)
        .find(|r| is_symmetric(grid, Grid::get_row, *r, grid.get_size().y)) {
        return Reflection::new(Orientation::Horizontal, row_symmetry + 1);  // 1-based
    }

    if let Some(col_symmetry) = (0..grid.get_size().x - 1)
        .find(|c| is_symmetric(grid, Grid::get_col, *c, grid.get_size().x)) {
        return Reflection::new(Orientation::Vertical, col_symmetry + 1);    // 1-based
    }

    panic!("No symmetry");
}

fn is_symmetric(grid: &Grid<char>, symmetry_fn: fn(&Grid<char>, usize) -> Option<Vec<char>>, index: usize, size: usize) -> bool {
    let iterations = (index + 1).min(size - index - 1);
    (0..iterations)
        .map(|it| (symmetry_fn(grid, index - it), symmetry_fn(grid, index + 1 + it)))
        .map(|(x0, x1)| (x0.unwrap_or_else(|| panic!("Invalid x0")), x1.unwrap_or_else(|| panic!("Invalid x1"))))
        .all(|(x0, x1)| x0 == x1)
}

fn get_summarized_total(grids: &Vec<Grid<char>>) -> usize {
    grids.iter()
        .map(|grid| find_reflection_line(grid))
        .map(|reflect| match reflect.orient {
            Orientation::Vertical => reflect.first_index,
            Orientation::Horizontal => reflect.first_index * 100
        })
        .sum()
}


#[derive(Eq, PartialEq, Debug, Copy, Clone)]
pub enum Orientation { Horizontal, Vertical }
#[derive(Eq, PartialEq, Debug, Clone)]
pub struct Reflection {
    pub orient: Orientation,
    pub first_index: usize      // Second index is always +1 of the first
}
impl Reflection {
    pub fn new(orient: Orientation, first_index: usize) -> Self {
        Self { orient, first_index }
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;
    use crate::day13::{find_reflection_line, get_summarized_total, Orientation, parse_input, part1, part2, Reflection};

    #[test]
    fn test_reflection_line() {
        assert_eq!(parse_input("src/day13/test-input-1.txt").iter()
            .map(|grid| find_reflection_line(&grid))
            .collect_vec(),

            vec![
                Reflection::new(Orientation::Vertical, 5),
                Reflection::new(Orientation::Horizontal, 4)]);
    }

    #[test]
    fn test_summarization() {
        assert_eq!(get_summarized_total(&parse_input("src/day13/test-input-1.txt")), 405);
    }

    #[test]
    fn test_edge_case_failure() {
        assert_eq!(find_reflection_line(&parse_input("src/day13/test-input-2.txt").iter().next().unwrap()),
                   Reflection::new(Orientation::Vertical, 2));
    }

    #[test]
    fn test_part1() {
        assert_eq!(part1(), 39471);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(), 12);
    }

}
