use std::fmt::{Display, Formatter};
use std::iter::Iterator;
use itertools::Itertools;
use crate::common::grid::{Grid, GridDirection};
use crate::common::vec2::Vec2;
use super::common;

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> usize {
    fill_trench(&dig_trench(&parse_input("src/day18/problem-input.txt"))).raw_data().iter()
        .filter(|x| x.is_dug)
        .count()
}

fn part2() -> usize {
    12
}

fn dig_trench(instructions: &Vec<Instruction>) -> Grid<Cell> {
    let cells = generate_trench_cells(instructions);
    let (min_bound, max_bound) = cells.iter()
        .fold((Vec2::new_uniform(0), Vec2::new_uniform(0)), |(min, max), x|
            (min.component_min(&x.pos), max.component_max(&x.pos)));

    // Extend bounds so that there is a one-cell border around the extend.  Makes flood fill simple to validate since we
    // know outer cells should not end up filled
    let grid_max = (max_bound - min_bound) + Vec2::new_uniform(2);
    let pos_offset = (min_bound * -1) + Vec2::new_uniform(1);

    let mut grid = Grid::new(Vec2::new(grid_max.x as usize + 1, grid_max.y as usize + 1), &Cell::new_empty());
    cells.iter().for_each(|cell| {
        let grid_pos = cell.pos + pos_offset;
        grid.set_at_coord(&Vec2::new(grid_pos.x as usize, grid_pos.y as usize), &Cell::new_dug(cell.color.to_string()));
    });

    grid
}

fn generate_trench_cells(instructions: &Vec<Instruction>) -> Vec<Trench> {
    let mut current = Vec2::new(0isize, 0);
    let mut trench = vec![Trench::new(Vec2::new(0, 0), NO_COLOR.to_string())];

    for inst in instructions {
        let unit_move = inst.dir.unit_movement();
        trench.extend((1isize..=inst.dist as isize)
              .map(|d| Trench::new(current + unit_move * d, inst.color.clone())));

        current += unit_move * inst.dist as isize;
    }

    trench
}

fn fill_trench(grid: &Grid<Cell>) -> Grid<Cell> {
    let mut filled = grid.clone();

    let fill_point = grid.get_size() / Vec2::new_uniform(2);
    filled.flood_fill(grid.coord_to_ix(&fill_point),
                      |cell| cell.is_dug = true,
                      |g, from, to, val| !val.is_dug);

    assert!(!grid.get(0).is_dug);

    filled
}

fn parse_input(file: &str) -> Vec<Instruction> {
    common::read_file(file)
        .lines()
        .map(|s| s.split_ascii_whitespace().collect_tuple::<(&str, &str, &str)>().unwrap_or_else(|| panic!("Invalid input format")))
        .map(|(dir, dist, color)| Instruction::new(
            get_dir(dir.chars().next().unwrap_or_else(|| panic!("Invalid input direction"))),
            dist.parse::<u32>().unwrap_or_else(|_| panic!("Invalid input distance")),
            color.to_string()))
        .collect_vec()
}

const NO_COLOR : &'static str = "None";

fn get_dir(c: char) -> GridDirection {
    match c {
        'L' => GridDirection::Left,
        'U' => GridDirection::Up,
        'R' => GridDirection::Right,
        'D' => GridDirection::Down,
        _ => panic!("Unknown direction '{}'", c)
    }
}

#[derive(Clone, Debug)]
struct Trench {
    pos: Vec2<isize>,
    color: String
}

impl Trench {
    pub fn new(pos: Vec2<isize>, color: String) -> Self {
        Self { pos, color }
    }
}

#[derive(Clone, Debug)]
struct Instruction {
    dir: GridDirection,
    dist: u32,
    color: String
}

impl Instruction {
    pub fn new(dir: GridDirection, dist: u32, color: String) -> Self {
        Self { dir, dist, color }
    }
}


#[derive(Clone, Debug)]
struct Cell {
    is_dug: bool,
    color: String
}

impl Cell {
    pub fn new_dug(color: String) -> Self {
        Self { is_dug: true, color }
    }
    pub fn new_empty() -> Self {
        Self { is_dug: false, color: NO_COLOR.to_string() }
    }
}
impl Display for Cell {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", if self.is_dug { '#' } else { '.' })
    }
}


#[cfg(test)]
mod tests {
    use crate::day18::{dig_trench, fill_trench, parse_input, part1, part2};

    #[test]
    fn test_excavation() {
        let grid = dig_trench(&parse_input("src/day18/test-input-1.txt"));
        let filled = fill_trench(&grid);
        assert_eq!(filled.raw_data().iter().filter(|x| x.is_dug).count(), 62);
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
