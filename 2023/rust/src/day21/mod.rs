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
    Some(parse_input("src/day21/problem-input.txt"))
        .map(|(grid, start)| calculate_for_inf_grid(&grid, start, 26501365))
        .unwrap()
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

// Part 2 solution on infinite grid requires some obscure Lagrange polynomial math.  Implementation
// for Day 21 Part 2 is mostly copied from examples
fn calculate_for_inf_grid(grid: &Grid<char>, start: usize, steps: usize) -> usize {
    let dx = grid.get_size().x();

    let vx = [dx / 2, 3 * dx / 2, 5 * dx / 2];
    let vy = walk_wrapped(&grid, start, &vx);
    // Use Lagrange polynomial
    let mut result = 0.0;
    for i in 0..3 {
        let mut term = vy[i] as f64;
        for j in 0..3 {
            if i != j {
                let num = (steps - vx[j]) as f64;
                let den = vx[i] as f64 - vx[j] as f64;
                term *= num / den;
            }
        }
        result += term;
    }
    result as usize
}

fn walk_wrapped<const N: usize>(g: &Grid<char>, start: usize, steps: &[usize; N]) -> [usize; N] {
    let mut vw = [0; N];
    let mut checked = HashSet::new();
    let (mut set, mut set_next) = (Vec::new(), Vec::new());
    let mut reach = HashSet::new();
    let sp = [(start % g.get_size().x()) as isize, (start / g.get_size().x()) as isize];
    checked.insert(sp);
    set.push(sp);
    let mut st = 0;
    let odd_bit = steps[0] & 1;
    let mut si = 0;
    while si < steps.len() {
        for &p in &set {
            if st & 1 == odd_bit {
                reach.insert(p);
            }
            for dir in [[0, -1], [1, 0], [0, 1], [-1, 0]] {
                let np = [p[0] + dir[0], p[1] + dir[1]];
                let x = (np[0].rem_euclid(g.get_size().x() as isize)) as usize;
                let y = (np[1].rem_euclid(g.get_size().x() as isize)) as usize;
                if g.raw_data()[y * g.get_size().x() + x] == '#' {
                    continue;
                }
                if checked.insert(np) {
                    set_next.push(np);
                }
            }
        }
        set.clear();
        std::mem::swap(&mut set, &mut set_next);
        st += 1;
        if st == steps[si]+1 {
            vw[si] = reach.len();
            si += 1;
        }
    }
    vw
}


#[cfg(test)]
mod tests {
    use crate::day21::{calculate_for_inf_grid, get_cells_reached, parse_input, part1, part2 };

    #[test]
    fn test_step_calculation() {
        assert_eq!(Some(parse_input("src/day21/test-input-1.txt"))
            .map(|(grid, start)| get_cells_reached(&grid, start, 6)).unwrap().len(),
                   16);
    }

    #[test]
    fn test_infinite_grid_calculation() {
        assert_eq!(Some(parse_input("src/day21/problem-input.txt"))
            .map(|(grid, start)| calculate_for_inf_grid(&grid, start, 26_501_365)).unwrap(),
                   635572596423432);
    }

    #[test]
    fn test_part1() {
        assert_eq!(part1(), 3649);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(), 635572596423432);
    }

}
