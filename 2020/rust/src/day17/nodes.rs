use crate::common::nvec::{Vec3, Vec4};
use crate::day17::space::SpatialNode;
use itertools::Itertools;

pub type Coord3 = Vec3<isize>;
pub type Coord4 = Vec4<isize>;

#[derive(Debug, Eq, Hash, PartialEq, PartialOrd, Clone)]
pub struct Cube {
    pos: Coord3
}

impl Cube {
    pub fn new(pos: Coord3) -> Self {
        Self { pos }
    }
}

impl SpatialNode for Cube {
    type Coord = Coord3;

    fn new(pos: Self::Coord) -> Self { Cube::new(pos) }
    fn get_position(&self) -> &Self::Coord { &self.pos }

    fn get_neighbours(&self) -> Vec<Self::Coord> {
        (0..3).map(|_| (-1isize..=1)).multi_cartesian_product()
            .map(|x: Vec<isize>| (x[0], x[1], x[2]))
            .filter(|&(x, y, z)| !(x == 0 && y == 0 && z == 0))
            .map(|(x,y,z)| self.pos.clone() + Self::Coord::new([x, y, z]))
            .collect()
    }
}


#[derive(Debug, Eq, Hash, PartialEq, PartialOrd, Clone)]
pub struct HyperCube {
    pos: Coord4
}

impl HyperCube {
    pub fn new(pos: Coord4) -> Self {
        Self { pos }
    }
}

impl SpatialNode for HyperCube {
    type Coord = Coord4;

    fn new(pos: Self::Coord) -> Self { HyperCube::new(pos) }
    fn get_position(&self) -> &Self::Coord { &self.pos }

    fn get_neighbours(&self) -> Vec<Self::Coord> {
        (0..4).map(|_| (-1isize..=1)).multi_cartesian_product()
            .map(|x: Vec<isize>| (x[0], x[1], x[2], x[3]))
            .filter(|&(x, y, z, w)| !(x == 0 && y == 0 && z == 0 && w == 0))
            .map(|(x,y,z, w)| self.pos.clone() + Self::Coord::new([x, y, z, w]))
            .collect()
    }
}

