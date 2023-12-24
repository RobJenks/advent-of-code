mod bricks;

use std::collections::HashSet;
use std::iter::Iterator;
use itertools::Itertools;
use crate::common::grid3d::Grid3d;
use crate::common::vec::{Vec2, Vec3};
use crate::day22::bricks::{Brick, BrickRegionIter, BrickRelations, NO_BRICK};
use super::common;
use super::common::num::Zero;

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> usize {
    Some(parse_input("src/day22/problem-input.txt"))
        .map(|bricks| get_removable_bricks(&bricks))
        .unwrap_or_else(|| panic!("No solution"))
        .len()
}

fn part2() -> usize {
    Some(parse_input("src/day22/problem-input.txt"))
        .map(|bricks| get_total_displacement(&bricks))
        .unwrap_or_else(|| panic!("No solution"))
}

fn get_removable_bricks(input_bricks: &Vec<Brick>) -> Vec<u32> {
    let initial_grid = build_grid(&input_bricks);
    let (grid, bricks) = apply_gravity(&initial_grid, input_bricks).unwrap_or_else(|| panic!("No movement"));
    let relations = get_brick_relations(&grid, &bricks);
    relations.supports.iter().enumerate()
        .filter(|&(_, supported_bricks)| supported_bricks.iter()
            .all(|&supported| relations.supported_by[supported as usize].len() != 1))
        .map(|(ix, _)| ix as u32)
        .collect_vec()
}

fn get_total_displacement(input_bricks: &Vec<Brick>) -> usize {
    let initial_grid = build_grid(&input_bricks);
    let (grid, bricks) = apply_gravity(&initial_grid, input_bricks).unwrap_or_else(|| panic!("No movement"));
    bricks.iter()
        .map(|b| get_bricks_displaced(&grid, &bricks, b.id))
        .sum()
}

fn get_bricks_displaced(input_grid: &Grid3d<u32>, input_bricks: &Vec<Brick>, removed: u32) -> usize {
    let (mut grid, mut bricks) = (input_grid.clone(), input_bricks.clone());

    remove_brick(&mut grid, &bricks[removed as usize]);
    bricks[removed as usize].start = Vec3::new(0, 0, 1);
    bricks[removed as usize].end = Vec3::new(0, 0, 1);

    while let Some((new_grid, new_bricks)) = apply_gravity(&grid, &bricks) {
        grid = new_grid;
        bricks = new_bricks;
    }

    bricks.iter().zip(input_bricks.iter())
        .filter(|&(new, old)| new != old)
        .count() - 1
}

fn apply_gravity(input: &Grid3d<u32>, input_bricks: &Vec<Brick>) -> Option<(Grid3d<u32>, Vec<Brick>)> {
    let mut grid = input.clone();
    let sorted = input_bricks.iter()
        .sorted_by(|b0, b1| b0.start.z().cmp(&b1.start.z()))
        .map(|b| b.id)
        .collect_vec();

    let mut change = false;
    let mut bricks = input_bricks.clone();
    for ix in sorted {
        let brick = bricks.get_mut(ix as usize).unwrap();
        let min_z = brick.start.z().min(brick.end.z());
        let new_base = lowest_level_below_region(&grid, brick.start.to_dimension::<2>(), brick.end.to_dimension::<2>(), min_z);

        let fall_dist = min_z - new_base;
        if fall_dist == 0 { continue }

        change = true;
        remove_brick(&mut grid, &brick);

        brick.start.data[2] -= fall_dist;
        brick.end.data[2] -= fall_dist;

        add_brick(&mut grid, &brick);
    }


    if change { Some((grid, bricks)) } else { None }
}

fn lowest_level_below_region(grid: &Grid3d<u32>, region_start: Vec2<usize>, region_end_incl: Vec2<usize>, region_z: usize) -> usize {
    let mut highest = 1usize;
    for x in region_start.x()..=region_end_incl.x() {
        for y in region_start.y()..=region_end_incl.y() {
            highest = highest.max((0..region_z).rev()
                        .filter(|&z| grid.get_at_coords(x, y, z) != NO_BRICK)
                        .map(|collision| collision + 1)
                        .next().unwrap_or_else(|| 1));
        }
    }

    highest
}

fn build_grid(bricks: &Vec<Brick>) -> Grid3d<u32> {
    let size = Vec3::new_uniform(1usize) + bricks.iter()
        .fold(Vec3::zero(), |acc, brick| acc.component_max(&brick.start).component_max(&brick.end));

    let mut grid = Grid3d::new(size, &NO_BRICK);
    bricks.iter().for_each(|b| add_brick(&mut grid, b));

    grid
}

fn add_brick(grid: &mut Grid3d<u32>, brick: &Brick) {
    for pos in BrickRegionIter::new(brick) {
        assert_eq!(grid.get_at_coord(&pos), NO_BRICK);
        grid.set_at_coord(&pos, &brick.id);
    }
}

fn remove_brick(grid: &mut Grid3d<u32>, brick: &Brick) {
    for pos in BrickRegionIter::new(brick) {
        assert_ne!(grid.get_at_coord(&pos), NO_BRICK);
        grid.set_at_coord(&pos, &NO_BRICK);
    }
}

fn get_brick_relations(grid: &Grid3d<u32>, bricks: &Vec<Brick>) -> BrickRelations {
    let (mut supports, mut supported_by) = (
        vec![HashSet::<u32>::new(); bricks.len()], vec![HashSet::<u32>::new(); bricks.len()]);

    for (ix, brick) in bricks.iter().enumerate() {
        for pos in BrickRegionIter::new(brick) {
            if let Some(below) = grid.get_coord_below_value(&pos) {
                if below != NO_BRICK && below != brick.id {
                    supports[below as usize].insert(ix as u32);
                    supported_by[ix].insert(below);
                }
            }
        }
    }

    BrickRelations::new(
        supports.iter().map(|h| h.to_owned().into_iter().collect_vec()).collect_vec(),
        supported_by.iter().map(|h| h.to_owned().into_iter().collect_vec()).collect_vec())
}

fn parse_input(file: &str) -> Vec<Brick> {
    common::read_file(file)
        .lines()
        .enumerate()
        .map(|(id, line)| (id as u32, scan_fmt!(line, "{d},{d},{d}~{d},{d},{d}", usize, usize, usize, usize, usize, usize)
            .unwrap_or_else(|_| panic!("Invalid input format"))))
        .map(|(id, (x0,y0,z0,x1,y1,z1))| Brick::new(id, Vec3::new(x0, y0, z0), Vec3::new(x1, y1, z1)))
        .collect_vec()
}


#[cfg(test)]
mod tests {
    use crate::day22::{ get_removable_bricks, get_total_displacement, parse_input, part1, part2};

    #[test]
    fn test_support_calculation() {
        assert_eq!(
            Some(parse_input("src/day22/test-input-1.txt"))
                .map(|bricks| get_removable_bricks(&bricks))
                .unwrap_or_else(|| panic!("No solution"))
                .len(), 5);
    }

    #[test]
    fn test_displacement_calculation() {
        assert_eq!(
            Some(parse_input("src/day22/test-input-1.txt"))
                .map(|bricks| get_total_displacement(&bricks))
                .unwrap_or_else(|| panic!("No solution")),
            7);
    }

    #[test]
    fn test_part1() {
        assert_eq!(part1(), 405);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(), 61297);
    }

}
