use std::fmt::{Display, Formatter};
use itertools::Itertools;
use crate::common::vec::Vec3;

pub struct Grid3d<T>
    where T: Clone + Display {

    size: Vec3<usize>,
    data: Vec<T>
}

impl<T> Grid3d<T>
    where T: Clone + Display {

    pub fn new(size: Vec3<usize>, initializer: &T) -> Self {
        Self {
            size,
            data: vec![initializer.clone(); size.product()]
        }
    }

    pub fn new_with_data(size: Vec3<usize>, data: &Vec<T>) -> Self {
        if !data.len() == (size.product()) { panic!("Invalid data size"); }
        Self {
            size,
            data: data.clone()
        }
    }

    pub fn new_with_owned_data(size: Vec3<usize>, data: Vec<T>) -> Self {
        if !data.len() == (size.product()) { panic!("Invalid data size"); }
        Self { size, data }
    }

    pub fn coord_to_ix(&self, coord: &Vec3<usize>) -> usize {
        coord.x() + (self.size.x() * (coord.y() + (coord.z() * self.size.y())))
    }

    pub fn coords_to_ix(&self, x: usize, y: usize, z: usize) -> usize {
        x + (self.size.x() * (y + (z * self.size.y())))
    }

    pub fn ix_to_coord(&self, ix: usize) -> Vec3<usize> {
        let xy = self.size.x() * self.size.y();
        let z = ix / xy;

        let xy_index = ix - (z * xy);
        let y = xy_index / self.size.x();
        let x = xy_index % self.size.x();

        Vec3::new(x, y, z)
    }


    pub fn get(&self, ix: usize) -> T {
        self.data[ix].clone()
    }

    pub fn get_ref(&self, ix: usize) -> &T {
        &self.data[ix]
    }

    pub fn get_at_coord(&self, coord: &Vec3<usize>) -> T {
        self.data[self.coord_to_ix(coord)].clone()
    }

    pub fn get_at_coords(&self, x: usize, y: usize, z: usize) -> T {
        self.data[self.coords_to_ix(x, y, z)].clone()
    }

    pub fn set(&mut self, ix: usize, val: &T) {
        self.data[ix] = val.clone();
    }

    pub fn set_owned(&mut self, ix: usize, val: T) {
        self.data[ix] = val;
    }

    pub fn set_at_coord(&mut self, coord: &Vec3<usize>, val: &T) {
        let ix = self.coord_to_ix(coord);
        self.data[ix] = val.clone();
    }

    pub fn set_at_coords(&mut self, x: usize, y: usize, z: usize, val: &T) {
        let ix = self.coords_to_ix(x, y, z);
        self.data[ix] = val.clone();
    }

    pub fn apply_at_coord(&mut self, coord: &Vec3<usize>, f: fn(&T) -> T) {
        let ix = self.coord_to_ix(coord);
        self.data[ix] = f(&self.data[ix]);
    }

    pub fn raw_data(&self) -> &Vec<T> {
        &self.data
    }

    pub fn get_size(&self) -> Vec3<usize> {
        self.size.clone()
    }

    pub fn get_element_count(&self) -> usize {
        self.size.product()
    }

    pub fn is_valid_coord(&self, coord: &Vec3<usize>) -> bool {
        coord.x() < self.size.x() && coord.y() < self.size.y() && coord.z() < self.size.z()
    }

    pub fn is_coord_on_left_edge(&self, coord: &Vec3<usize>) -> bool {
        coord.x() == 0
    }
    pub fn is_coord_on_right_edge(&self, coord: &Vec3<usize>) -> bool {
        coord.x() == (self.size.x() - 1)
    }
    pub fn is_coord_on_top_edge(&self, coord: &Vec3<usize>) -> bool {
        coord.y() == 0
    }
    pub fn is_coord_on_bottom_edge(&self, coord: &Vec3<usize>) -> bool {
        coord.y() == (self.size.y() - 1)
    }
    pub fn is_coord_on_upper_edge(&self, coord: &Vec3<usize>) -> bool {
        coord.z() == 0
    }
    pub fn is_coord_on_lower_edge(&self, coord: &Vec3<usize>) -> bool {
        coord.z() == (self.size.z() - 1)
    }

    pub fn get_coord_left(&self, coord: &Vec3<usize>) -> Option<Vec3<usize>> {
        if self.is_coord_on_left_edge(coord) { None } else { Some(Vec3::new(coord.x() - 1, coord.y(), coord.z())) }
    }
    pub fn get_coord_up(&self, coord: &Vec3<usize>) -> Option<Vec3<usize>> {
        if self.is_coord_on_top_edge(coord) { None } else { Some(Vec3::new(coord.x(), coord.y() - 1, coord.z())) }
    }
    pub fn get_coord_right(&self, coord: &Vec3<usize>) -> Option<Vec3<usize>> {
        if self.is_coord_on_right_edge(coord) { None } else { Some(Vec3::new(coord.x() + 1, coord.y(), coord.z())) }
    }
    pub fn get_coord_down(&self, coord: &Vec3<usize>) -> Option<Vec3<usize>> {
        if self.is_coord_on_bottom_edge(coord) { None } else { Some(Vec3::new(coord.x(), coord.y() + 1, coord.z())) }
    }
    pub fn get_coord_above(&self, coord: &Vec3<usize>) -> Option<Vec3<usize>> {
        if self.is_coord_on_upper_edge(coord) { None } else { Some(Vec3::new(coord.x(), coord.y(), coord.z() + 1)) }
    }
    pub fn get_coord_below(&self, coord: &Vec3<usize>) -> Option<Vec3<usize>> {
        if self.is_coord_on_lower_edge(coord) { None } else { Some(Vec3::new(coord.x(), coord.y(), coord.z() - 1)) }
    }


    pub fn get_coord_left_value(&self, coord: &Vec3<usize>) -> Option<T> {
        self.get_coord_left(coord).map(|adj| self.get_at_coord(&adj))
    }
    pub fn get_coord_up_value(&self, coord: &Vec3<usize>) -> Option<T> {
        self.get_coord_up(coord).map(|adj| self.get_at_coord(&adj))
    }
    pub fn get_coord_right_value(&self, coord: &Vec3<usize>) -> Option<T> {
        self.get_coord_right(coord).map(|adj| self.get_at_coord(&adj))
    }
    pub fn get_coord_down_value(&self, coord: &Vec3<usize>) -> Option<T> {
        self.get_coord_down(coord).map(|adj| self.get_at_coord(&adj))
    }
    pub fn get_coord_above_value(&self, coord: &Vec3<usize>) -> Option<T> {
        self.get_coord_above(coord).map(|adj| self.get_at_coord(&adj))
    }
    pub fn get_coord_below_value(&self, coord: &Vec3<usize>) -> Option<T> {
        self.get_coord_below(coord).map(|adj| self.get_at_coord(&adj))
    }

    pub fn get_surrounding(&self, coord: &Vec3<usize>) -> Vec<Vec3<usize>> {
        [self.get_coord_left(coord), self.get_coord_up(coord), self.get_coord_right(coord),
         self.get_coord_down(coord), self.get_coord_above(coord), self.get_coord_below(coord)]
            .iter()
            .flat_map(|s| *s)
            .collect_vec()
    }

    pub fn get_layer_start_index(&self, z: usize) -> usize {
        z * self.size.x() * self.size.y()
    }

    pub fn manhattan_dist(&self, from: &Vec3<usize>, to: &Vec3<usize>) -> usize {
        (  (to.x() as isize - from.x() as isize).abs() +
           (to.y() as isize - from.y() as isize).abs() +
           (to.z() as isize - from.z() as isize).abs()  ) as usize
    }

    pub fn layer_to_string(&self, z: usize) -> String {
        self.layer_to_string_fmt(z, &T::to_string)
    }

    pub fn layer_to_string_fmt(&self, z: usize, fmt: &impl Fn(&T) -> String) -> String {
        let layer_size = self.size.x() * self.size.y();
        self.data.iter()
            .skip(z * layer_size)
            .take(layer_size)
            .chunks(self.size.x()).into_iter().map(|chunk| chunk
            .map(|x| fmt(x)).join(""))
            .join("\n")
    }

    pub fn to_string(&self) -> String {
        self.to_string_fmt(&T::to_string)
    }
    pub fn to_string_fmt(&self, fmt: &impl Fn(&T) -> String) -> String {
        (0..self.size.z())
            .map(|z| self.layer_to_string_fmt(z, fmt))
            .join("\n\n")
    }
}

impl <T> Clone for Grid3d<T>
    where T: Clone + Display {

    fn clone(&self) -> Self {
        Self { size: self.size, data: self.data.clone() }
    }
}

#[repr(u32)]
#[derive(Eq, PartialEq, Copy, Clone, Debug, Hash)]
pub enum Grid3dDirection {
    Left = 0,
    Up = 1,
    Right = 2,
    Down = 3,
    Above = 4,
    Below = 5
}

const OPPOSITE_GRID_DIRECTIONS : [Grid3dDirection; 6] = [
    Grid3dDirection::Right,    // Opposite of Left (= 0)
    Grid3dDirection::Down,     // Opposite of Up (= 1)
    Grid3dDirection::Left,     // Opposite of Right (= 2)
    Grid3dDirection::Up,       // Opposite of Down (= 3)
    Grid3dDirection::Below,    // Opposite of Above (= 4)
    Grid3dDirection::Above,    // Opposite of Below (= 5)
];

const DIRECTION_UNIT_MOVEMENTS : [Vec3<isize>; 6] = [
    Vec3 { data: [-1, 0, 0] },    // Left (= 0)
    Vec3 { data: [0, -1, 0] },    // Up (= 1)
    Vec3 { data: [1, 0, 0] },     // Right (= 2)
    Vec3 { data: [0, 1, 0] },     // Down (= 3)
    Vec3 { data: [0, 0, 1] },     // Above (= 4)
    Vec3 { data: [0, 0, -1] }     // Below (= 5)
];

impl Grid3dDirection {
    pub fn unit_movement(&self) -> Vec3<isize> {
        DIRECTION_UNIT_MOVEMENTS[*self as usize]
    }

    pub fn opposite(&self) -> Grid3dDirection {
        OPPOSITE_GRID_DIRECTIONS[*self as usize]
    }

    pub fn directions() -> [Grid3dDirection; 6] {
        [Grid3dDirection::Left, Grid3dDirection::Up, Grid3dDirection::Down,
         Grid3dDirection::Right, Grid3dDirection::Above, Grid3dDirection::Below ]
    }
}

impl Display for Grid3dDirection {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}


#[cfg(test)]
mod tests {
    use itertools::Itertools;
    use crate::common::grid3d::Grid3d;
    use crate::common::vec::Vec3;
    use crate::common::num::Zero;

    #[test]
    fn test_coord_mapping() {
        let size = Vec3::new(2usize, 4, 6);
        let grid = Grid3d::new_with_data(size, &(0..size.product()).collect_vec());

        assert_eq!(grid.ix_to_coord(0), Vec3::zero());
        assert_eq!(grid.coord_to_ix(&Vec3::zero()), 0);

        assert_eq!(grid.ix_to_coord(size.product() - 1), size - Vec3::new_uniform(1usize));
        assert_eq!(grid.coord_to_ix(&(size - Vec3::new_uniform(1usize))), size.product() - 1);

        assert_eq!(grid.coord_to_ix(&Vec3::new(2, 2, 2)), 22);

        assert_eq!(grid.coord_to_ix(&grid.ix_to_coord(16)), 16);
    }
}
