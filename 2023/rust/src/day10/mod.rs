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
    get_dry_area_after_flooding(&parse_input("src/day10/problem-input.txt"))
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

fn flood_fill(maze: &Maze) -> Maze {
    let path = get_circular_path(maze);

    let expand_coord = |v: Vec2<usize>| (v * Vec2::<usize>::new(2, 2)) - Vec2::<usize>::new(1, 1);
    let exp_size = expand_coord(maze.size);

    #[allow(unstable_name_collisions)]  // Intersperse
    let data = (0..maze.size.y)
        .map(|_| vec!['.'; maze.size.x].iter().intersperse(&' ').cloned().collect_vec())
        .intersperse(vec![' '; exp_size.x])
        .collect_vec();

    let mut expanded = Maze::new(exp_size, data.iter()
        .flat_map(|v| v.iter())
        .cloned()
        .collect_vec());

    path.iter().for_each(|ix| {
        let val = maze.grid.get(*ix);
        let new_coord = maze.grid.ix_to_coord(*ix) * Vec2::new(2, 2);
        let new_ix = expanded.grid.coord_to_ix(&new_coord);
        expanded.grid.set(new_ix, &val);

        let dy = expanded.size.x;
        if val == '|' || val == 'F' || val == '7' {
            expanded.grid.set(new_ix + dy, &'|');
        }
        if val == '|' || val == 'L' || val == 'J' {
            expanded.grid.set(new_ix - dy, &'|');
        }
        if val == 'F' || val == '-' || val == 'L' {
            expanded.grid.set(new_ix + 1, &'-');
        }
        if val == '7' || val == '-' || val == 'J' {
            expanded.grid.set(new_ix - 1, &'-');
        }
    });

    let mut current = expanded;
    while let Some(start) = find_edge_fill_start(&current) {
        let mut flooded = Maze::new(current.size, current.grid.raw_data().clone());

        let flood = flooded.flood_fill_from(start);
        flood.iter().for_each(|ix| flooded.grid.set(*ix, &'#'));

        current = flooded;
    }

    current
}

fn find_edge_fill_start(maze: &Maze) -> Option<usize> {
    (0..maze.size.x).flat_map(|x| [Vec2::new(x, 0), Vec2::new(x, maze.size.y - 1)]).chain(
    (0..maze.size.y).flat_map(|y| [Vec2::new(0, y), Vec2::new(maze.size.x - 1, y)]))
        .map(|coord| maze.grid.coord_to_ix(&coord))
        .filter(|&ix| maze.grid.get(ix) == '.')
        .filter(|&ix| maze.grid.is_on_edge(ix))
        .next()
}

fn get_dry_area_after_flooding(maze: &Maze) -> usize {
    flood_fill(maze).grid.raw_data().iter().filter(|&c| *c == '.').count()
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
    use crate::day10::{get_circular_path, get_furthest_step_number, parse_input, part1, part2, get_dry_area_after_flooding};

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
    fn test_flood_fill_1() {
        assert_eq!(
            get_dry_area_after_flooding(&parse_input("src/day10/test-input-3.txt")),
            4);
    }

    #[test]
    fn test_flood_fill_2() {
        assert_eq!(
            get_dry_area_after_flooding(&parse_input("src/day10/test-input-4.txt")),
            1);
    }

    #[test]
    fn test_flood_fill_3() {
        assert_eq!(
            get_dry_area_after_flooding(&parse_input("src/day10/test-input-5.txt")),
            10);
    }

    #[test]
    fn test_part1() {
        assert_eq!(part1(), 6909);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(), 461);
    }
}
