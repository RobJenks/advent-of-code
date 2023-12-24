mod bricks;

use std::collections::HashSet;
use std::iter::Iterator;
use itertools::Itertools;
use crate::common::grid3d::{Grid3d, Grid3dDirection};
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
    12
}

fn get_removable_bricks(input_bricks: &Vec<Brick>) -> Vec<u32> {
    let initial_grid = build_grid(&input_bricks);
    let (grid, bricks) = apply_gravity(&initial_grid, input_bricks);
    let relations = get_brick_relations(&grid, &bricks);
    relations.supports.iter().enumerate()
        .filter(|&(_, supported_bricks)| supported_bricks.iter()
            .all(|&supported| relations.supported_by[supported as usize].len() != 1))
        .map(|(ix, _)| ix as u32)
        .collect_vec()
}

fn apply_gravity(input: &Grid3d<u32>, input_bricks: &Vec<Brick>) -> (Grid3d<u32>, Vec<Brick>) {
    let mut grid = input.clone();
    let sorted = input_bricks.iter()
        .sorted_by(|b0, b1| b0.start.z().cmp(&b1.start.z()))
        .map(|b| b.id)
        .collect_vec();

    //sorted.iter().map(|ix| &input_bricks[*ix as usize]).for_each(|b| println!("{} = {}-{}", b.id, b.start, b.end));

    let mut bricks = input_bricks.clone();
    for ix in sorted {
        let brick = bricks.get_mut(ix as usize).unwrap();
        let min_z = brick.start.z().min(brick.end.z());
        let new_base = lowest_level_below_region(&grid, brick.start.to_dimension::<2>(), brick.end.to_dimension::<2>(), min_z);

        let fall_dist = min_z - new_base;
        if fall_dist == 0 { continue }

        remove_brick(&mut grid, &brick);

        brick.start.data[2] -= fall_dist;
        brick.end.data[2] -= fall_dist;

        add_brick(&mut grid, &brick);
    }


    (grid, bricks)
}

fn bricks_overlapping_layer(grid: &Grid3d<u32>, z: usize) -> Vec<u32> {
    let st = grid.get_layer_start_index(z);
    (st..(st + grid.get_size().x() * grid.get_size().y()))
        .map(|ix| grid.get(ix))
        .filter(|v| *v != NO_BRICK)
        .unique()
        .collect_vec()
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

fn grid_fmt(val: &u32, max_value_size: usize) -> String {
    let s = match val {
        &NO_BRICK => ".".to_string(),
        _ => val.to_string()
    };

    let space = max_value_size + 2;
    let value_len = s.len();
    let pad_left = (space - value_len) / 2;
    let pad_right = space - pad_left - 1;

    format!("{}{}{}", String::from(' ').repeat(pad_left), s, String::from(' ').repeat(pad_right))
}

fn grid_to_string(grid: &Grid3d<u32>, bricks: &Vec<Brick>) -> String {
    let max_size = bricks.len().to_string().len();
    let fmt = |v: &u32| grid_fmt(v, max_size);

    (0..grid.get_size().z())
        .rev()
        .map(|z| grid.layer_to_string_fmt(z, &fmt))
        .join("\n\n")
}

#[cfg(test)]
mod tests {
    use crate::day22::{apply_gravity, build_grid, get_removable_bricks, grid_to_string, parse_input, part1, part2};

    #[test]
    fn test_fall_calculation() {
        let bricks = parse_input("src/day22/test-input-1.txt");
        let grid = build_grid(&bricks);
        let applied = apply_gravity(&grid, &bricks);

        println!("{}", grid_to_string(&grid, &bricks));
        println!("\n---\n{}", grid_to_string(&applied.0, &applied.1));
    }

    #[test]
    fn test_support_calculation() {
        assert_eq!(
            Some(parse_input("src/day22/test-input-1.txt"))
                .map(|brick| get_removable_bricks(&brick))
                .unwrap_or_else(|| panic!("No solution"))
                .len(), 5);
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
