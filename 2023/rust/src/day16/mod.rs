use std::iter::Iterator;
use itertools::Itertools;
use crate::common::grid::{Grid, GridDirection};
use super::common;

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> usize {
    simulate_energized_cells(&parse_input("src/day16/problem-input.txt"))
}

fn part2() -> usize {
    12
}

fn simulate_energized_cells(grid: &Grid<char>) -> usize {
    let mut energized : Vec::<[bool; 4]> = vec![[false; 4]; grid.get_element_count()];
    let mut spawned = Vec::<Beam>::new();

    let mut beams = vec![Beam::new_off_grid(0, GridDirection::Right)];
    energized[0][GridDirection::Left as usize] = false;

    while !beams.is_empty() || !spawned.is_empty() {
        for beam in &mut beams {
            let next_ix = if beam.off_grid { Some(beam.pos) } else { grid.get_in_direction(beam.pos, beam.dir) };
            beam.off_grid = false;

            if let Some(new_cell) = next_ix {
                if energized[new_cell][beam.dir as usize] {
                    beam.pos = usize::MAX; // Kill since we are following the exact path of a previous beam
                }
                else {
                    if !beam.off_grid {
                        energized[new_cell][beam.dir as usize] = true;
                    }
                    beam.pos = new_cell;

                    let val = grid.get(new_cell);
                    if val == '.' { /* Nothing to do */ }
                    else if val == '/' {
                        beam.dir = FWD_REFLECT[beam.dir as usize];
                    } else if val == '\\' {
                        beam.dir = BWD_REFLECT[beam.dir as usize];
                    } else if val == '|' {
                        if SHOULD_VSPLIT[beam.dir as usize] {
                            beam.dir = GridDirection::Up;
                            spawned.push(Beam::new(beam.pos, GridDirection::Down));
                        }
                    } else if val == '-' {
                        if SHOULD_HSPLIT[beam.dir as usize] {
                            beam.dir = GridDirection::Left;
                            spawned.push(Beam::new(beam.pos, GridDirection::Right));
                        }
                    }
                }
            }
            else {
                beam.pos = usize::MAX;  // Kill beam
            }
        }

        beams = beams.into_iter().filter(|beam| beam.pos != usize::MAX).collect_vec();

        spawned.iter().for_each(|new_beam| beams.push(new_beam.clone()));
        spawned.clear();
    }

    energized.iter().filter(|&en| en.iter().any(|x| *x)).count()
}

const FWD_REFLECT : [GridDirection; 4] = [  // '/'
    GridDirection::Down,  // Beam travelling Left
    GridDirection::Right, // Beam travelling Up
    GridDirection::Up,    // Beam travelling Right
    GridDirection::Left   // Beam travelling Down
];

const BWD_REFLECT : [GridDirection; 4] = [  // '\'
    GridDirection::Up,    // Beam travelling Left
    GridDirection::Left,  // Beam travelling Up
    GridDirection::Down,  // Beam travelling Right
    GridDirection::Right  // Beam travelling Down
];

const SHOULD_VSPLIT : [bool; 4] = [ // '|'
    true,   // Beam travelling Left
    false,  // Beam travelling Up
    true,   // Beam travelling Right
    false   // Beam travelling Down
];

const SHOULD_HSPLIT : [bool; 4] = [ // '-`
    false,  // Beam travelling Left
    true,   // Beam travelling Up
    false,  // Beam travelling Right
    true    // Beam travelling Down
];

fn parse_input(file: &str) -> Grid<char> {
    Grid::new_from_2d_data(
        &common::read_file(file)
            .lines()
            .map(|s| s.trim().chars().collect_vec())
            .collect_vec())
}

#[derive(Eq, PartialEq, Clone, Debug)]
struct Beam {
    pos: usize,
    dir: GridDirection,
    off_grid: bool
}

impl Beam {
    pub fn new(pos: usize, dir: GridDirection) -> Self {
        Self { pos, dir, off_grid: false }
    }
    pub fn new_off_grid(next_pos: usize, dir: GridDirection) -> Self {
        Self { pos: next_pos, dir, off_grid: true }
    }
}

#[cfg(test)]
mod tests {
    use crate::day16::{parse_input, part1, part2, simulate_energized_cells};

    #[test]
    fn test_beam_traversal() {
        assert_eq!(simulate_energized_cells(&parse_input("src/day16/test-input-1.txt")), 46);
    }

    #[test]
    fn test_part1() {
        assert_eq!(part1(), 7111);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(), 12);
    }

}
