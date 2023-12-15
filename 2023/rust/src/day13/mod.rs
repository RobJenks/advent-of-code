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
    get_summarized_total(&parse_input("src/day13/problem-input.txt"), find_reflection_line)
}

fn part2() -> usize {
    get_summarized_total(&parse_input("src/day13/problem-input.txt"), calculate_smudged_reflection)
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

fn get_summarized_total(grids: &Vec<Grid<char>>, reflect_fn: fn(&Grid<char>) -> Reflection) -> usize {
    grids.iter()
        .map(|grid| reflect_fn(grid))
        .map(|reflect| match reflect.orient {
            Orientation::Vertical => reflect.first_index,
            Orientation::Horizontal => reflect.first_index * 100
        })
        .sum()
}

fn smudge(grid: &mut Grid<char>, ix: usize) {
    let current = grid.get(ix);
    grid.set(ix, if current == '.' { &'#' } else { &'.' });
}

fn find_alternate_reflection(grid: &Grid<char>, current: &Reflection) -> Option<Reflection> {
    if let Some(row_symmetry) = (0..grid.get_size().y - 1)
        .filter(|&r| !(current.orient == Orientation::Horizontal && r == (current.first_index - 1)))
        .filter(|&r| is_symmetric(grid, Grid::get_row, r, grid.get_size().y))
        .next() {
        return Some(Reflection::new(Orientation::Horizontal, row_symmetry + 1));  // 1-based
    }

    if let Some(col_symmetry) = (0..grid.get_size().x - 1)
        .filter(|&c| !(current.orient == Orientation::Vertical && c == (current.first_index - 1)))
        .filter(|&c| is_symmetric(grid, Grid::get_col, c, grid.get_size().x))
        .next() {
        return Some(Reflection::new(Orientation::Vertical, col_symmetry + 1));  // 1-based
    }

    None
}

fn calculate_smudged_reflection(grid: &Grid<char>) -> Reflection {
    let mut test_grid = grid.clone();
    let current_reflection = find_reflection_line(&test_grid);
    let n = test_grid.raw_data().len();

    for i in 0..n {
        smudge(&mut test_grid, i);
        if let Some(reflect) = find_alternate_reflection(&test_grid, &current_reflection) {
            return reflect;
        }
        smudge(&mut test_grid, i);
    }

    panic!("No smudged reflection");
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
    use crate::day13::{calculate_smudged_reflection, find_reflection_line, get_summarized_total, Orientation, parse_input, part1, part2, Reflection};

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
        assert_eq!(get_summarized_total(&parse_input("src/day13/test-input-1.txt"), find_reflection_line), 405);
    }

    #[test]
    fn test_edge_case_failure() {
        assert_eq!(find_reflection_line(&parse_input("src/day13/test-input-2.txt").iter().next().unwrap()),
                   Reflection::new(Orientation::Vertical, 2));
    }

    #[test]
    fn test_smudge() {
        assert_eq!(parse_input("src/day13/test-input-1.txt").iter_mut()
                       .map(|grid| calculate_smudged_reflection(grid))
                       .collect_vec(),

                   vec![
                       Reflection::new(Orientation::Horizontal, 3),
                       Reflection::new(Orientation::Horizontal, 1)]);
    }

    #[test]
    fn test_part1() {
        assert_eq!(part1(), 37381);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(), 28210);
    }

}
