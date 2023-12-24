use itertools::Itertools;
use crate::common::vec::Vec3;

pub const NO_BRICK: u32 = u32::MAX;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Brick {
    pub id: u32,
    pub start: Vec3<usize>,
    pub end: Vec3<usize>
}
impl Brick {
    pub fn new(id: u32, start: Vec3<usize>, end: Vec3<usize>) -> Self {
        Self { id, start, end }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct BrickRelations {
    pub supports: Vec<Vec<u32>>,     // X -> List of bricks supported by X
    pub supported_by: Vec<Vec<u32>>  // X -> List of bricks which support X
}
impl BrickRelations {
    pub fn new(supports: Vec<Vec<u32>>, supported_by: Vec<Vec<u32>>) -> Self {
        Self { supports, supported_by }
    }
}

pub struct BrickRegionIter {
    region: Vec<Vec3<usize>>,
    ix: usize
}

impl BrickRegionIter {
    pub fn new(brick: &Brick) -> Self {
        let min = brick.start.component_min(&brick.end);
        let max = brick.start.component_max(&brick.end);

        let region = (min.x()..=max.x()).cartesian_product(
            min.y()..=max.y()).cartesian_product(
            min.z()..=max.z())

            .map(|((x, y), z)| Vec3::new(x, y, z))
            .collect_vec();

        Self { region, ix: 0 }
    }
}

impl Iterator for BrickRegionIter {
    type Item = Vec3<usize>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.ix >= self.region.len() { None }
        else {
            let ret = self.region[self.ix];
            self.ix += 1;
            Some(ret)
        }
    }
}
