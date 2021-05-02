mod nodes;
mod space;
use crate::common;
use crate::common::nvec::Vec3;
use crate::day17::nodes::{Coord3, Cube};
use crate::day17::space::{Space, SpatialNode};

pub fn run() {
    println!("Part 1 result: {}", part1());
}

fn part1() -> usize {
    parse_input(common::read_file("src/day17/problem-input.txt"))
        .execute_n(6)
        .get_active()
        .len()
}


fn parse_input(input: String) -> Space<Cube> {
    Space::new(
        input.lines().enumerate()
            .map(|(y,xs)| xs.chars().enumerate()
                .filter(|(_, c)| *c == '#')
                .map(|(x, _)| Vec3::new([x as isize, y as isize, 0]))
                .collect::<Vec<_>>())
            .flatten()
            .map(|pos| Cube::new(pos))
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
    use crate::day17::nodes_for;
    use itertools::Itertools;

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