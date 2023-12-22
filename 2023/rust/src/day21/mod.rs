use std::collections::HashSet;
use std::iter::Iterator;
use itertools::Itertools;
use crate::common::grid::{Grid, GridDirection};
use crate::common::vec::Vec2;
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

fn calculate_for_inf_grid(grid: &Grid<char>, start: usize, steps: usize) -> usize {
    // xmax/ymax=  grid.size
    // active = frontier
    // x1/y1 = p_mod


    let mut visited = HashSet::<Vec2<isize>>::new();
    let mut active = HashSet::<Vec2<isize>>::new();
    active.insert(Some(grid.ix_to_coord(start)).map(|v| Vec2::new(v.x() as isize, v.y() as isize)).unwrap());

    let mut count = [0usize, 0, 0];
    let mut frontiers = vec![0isize; grid.get_size().x()];

    let (mut d1, mut d2) = (vec![0isize; grid.get_size().x()], vec![0isize; grid.get_size().x()]);

    let mut step = 0usize;
    loop {
        let mut new_front = HashSet::<Vec2<isize>>::new();
        for p in &active {
            for d in GridDirection::directions() {
                let p_off = *p + d.unit_movement();
                let p_m = Vec2::new(p_off.x() % grid.get_size().x() as isize, p_off.y() % grid.get_size().y() as isize);
                let p_mod = Vec2::new(if p_m.x() >= 0 { p_m.x() } else { p_m.x() + grid.get_size().x() as isize },
                                      if p_m.y() >= 0 { p_m.y() } else { p_m.y() + grid.get_size().y() as isize });

                if grid.get_at_coords(p_mod.x() as usize, p_mod.y() as usize) == '#' {
                    if visited.insert(p_off) {
                        new_front.insert(p_off);
                    }
                }
            }
        }

        let front_len = new_front.len();
        count[2] = front_len + count[0];
        count[0] = count[1];
        count[1] = count[2];

        let ix = step % grid.get_size().x();
        if step >= grid.get_size().x() {
            let dx = front_len as isize - frontiers[ix];
            d2[ix] = dx - d1[dx as usize];
            d1[ix] = dx;
        }
        frontiers[ix] = front_len as isize;

        active = new_front;
        step += 1;

        if step >= (2 * grid.get_size().x()) {
            if d2.iter().all(|x| *x == 0) { break }
        }
    }

    for i in step..steps {
        let ix = i % grid.get_size().x();
        d1[ix] += d2[ix];
        frontiers[ix] += d1[ix];

        count[2] = count[0] + frontiers[ix] as usize;
        count[0] = count[1];
        count[1] = count[2];
    }

    println!("counts {:?}", count);
    count[2]
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
println!("Start = {}", grid.ix_to_coord(start));
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
