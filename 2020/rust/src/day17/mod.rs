mod nodes;
mod space;
use crate::common;
use crate::common::nvec::{Vec3, Vec4};
use crate::day17::nodes::{Coord3, Cube, HyperCube};
use crate::day17::space::{Space, SpatialNode};

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> usize {
    parse_input::<Cube>(common::read_file("src/day17/problem-input.txt"), |x, y| Vec3::new([x, y, 0]))
        .execute_n(6)
        .get_active()
        .len()
}

fn part2() -> usize {
    parse_input::<HyperCube>(common::read_file("src/day17/problem-input.txt"), |x, y| Vec4::new([x, y, 0, 0]))
        .execute_n(6)
        .get_active()
        .len()
}


fn parse_input<T>(input: String, f: fn(isize, isize) -> T::Coord) -> Space<T>
    where T: SpatialNode {

    Space::new(
        input.lines().enumerate()
            .map(|(y,xs)| xs.chars().enumerate()
                .filter(|(_, c)| *c == '#')
                .map(|(x, _)| f(x as isize, y as isize))
                .collect::<Vec<_>>())
            .flatten()
            .map(|pos| T::new(pos))
            .collect::<Vec<_>>())
}


fn nodes_for<T: SpatialNode>(pos: Vec<T::Coord>) -> Vec<T> {
    pos.iter().map(|p| T::new(p.to_owned())).collect()
}

#[cfg(test)]
mod tests {
    use crate::day17::space::{Space, SpatialNode};
    use crate::day17::nodes::{Cube, Coord3};
    use crate::common::nvec::Vec3;
    use crate::day17::{nodes_for, part1};
    use itertools::Itertools;

    #[test]
    fn test_part1() {
        assert_eq!(301, part1());
    }

    #[test]
    fn test_basic_evolution() {
        let mut space = Space::new(nodes_for::<Cube>(vec![
            Vec3::new([1,0,0]), Vec3::new([2,1,0]), Vec3::new([0,2,0]), Vec3::new([1,2,0]), Vec3::new([2,2,0])]));
        assert_eq!(5, space.get_active().len());

        space = space.execute();
        assert_eq!(11, space.get_active().len());

        space = space.execute();
        assert_eq!(21, space.get_active().len());

        space = space.execute();
        assert_eq!(38, space.get_active().len());
    }

    #[test]
    fn test_full_cycle_evolution() {
        let space = Space::new(nodes_for::<Cube>(vec![
            Vec3::new([1,0,0]), Vec3::new([2,1,0]), Vec3::new([0,2,0]), Vec3::new([1,2,0]), Vec3::new([2,2,0])]));

        let new_space = space.execute_n(6);
        assert_eq!(112, new_space.get_active().len());
    }

}