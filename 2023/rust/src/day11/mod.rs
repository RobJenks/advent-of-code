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
    sum_shortest_paths(&expand_universe(&parse_input("src/day11/problem-input.txt")))
}

fn part2() -> usize {
    12
}

fn sum_shortest_paths(grid: &Grid<char>) -> usize {
    let galaxies = get_galaxy_positions(grid);
    galaxies.iter().enumerate()
        .skip(1)
        .map(|(g, pos)| galaxies.iter()
            .take(g)
            .map(|other_pos| calculate_distance(pos, other_pos))
            .sum::<usize>())
        .sum()
}

fn get_galaxy_positions(grid: &Grid<char>) -> Vec<Vec2<usize>> {
    grid.raw_data().iter().enumerate()
        .filter(|&(_, c)| c == &'#')
        .map(|(ix, _)| grid.ix_to_coord(ix))
        .collect_vec()
}

fn calculate_distance(p0: &Vec2<usize>, p1: &Vec2<usize>) -> usize {
    let mut dist = 0usize;
    let mut pos = Vec2::new(p0.x as isize, p0.y as isize);
    let target = Vec2::new(p1.x as isize, p1.y as isize);

    while pos != target {
        let (dx, dy) = (target.x - pos.x, target.y - pos.y);
        if dx.abs() >= dy.abs() { pos.x += dx.signum() }
        else { pos.y += dy.signum() }
        dist += 1
    }

    dist
}

fn expand_universe(grid: &Grid<char>) -> Grid<char> {
    let exp_rows = (0..grid.get_size().y)
        .map(|r| (r, grid.get_row(r).unwrap()))
        .filter(|(_, row)| !row.contains(&'#'))
        .map(|(r, _)| r)
        .collect_vec();

    let exp_cols = (0..grid.get_size().x)
        .map(|c| (c, grid.get_col(c).unwrap()))
        .filter(|(_, col)| !col.contains(&'#'))
        .map(|(c, _)| c)
        .collect_vec();

    let mut new_data: Vec<Vec<char>> = Vec::new();
    for r in 0..grid.get_size().y {
        let mut row = Vec::new();
        for c in 0..grid.get_size().x {
            let chr = grid.get_at_coords(c, r);
            row.push(chr);
            if exp_cols.contains(&c) {
                row.push(chr);
            }
        }

        if exp_rows.contains(&r) {
            new_data.push(row.clone());
        }
        new_data.push(row);
    }

    Grid::new_from_2d_data(&new_data)
}

fn parse_input(file: &str) -> Grid<char> {
    Grid::new_from_2d_data(
        &common::read_file(file)
            .lines()
            .map(|s| s.trim().chars().collect_vec())
            .collect_vec())
}

#[cfg(test)]
mod tests {
    use crate::day11::{expand_universe, parse_input, part1, part2, sum_shortest_paths};

    #[test]
    fn test_universe_expansion() {
        assert_eq!(expand_universe(&parse_input("src/day11/test-input-1.txt")).to_string(),
                   parse_input("src/day11/test-output-1.txt").to_string());

    }

    #[test]
    fn test_path_calculation() {
        assert_eq!(sum_shortest_paths(&expand_universe(&parse_input("src/day11/test-input-1.txt"))), 374);
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
