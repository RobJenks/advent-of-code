use std::fmt::Display;
use itertools::Itertools;
use crate::common::vec2::Vec2;

pub struct Grid<T>
    where T: Clone + Display {

    size: Vec2<usize>,
    data: Vec<T>
}

#[allow(dead_code)]
impl <T> Grid<T>
    where T: Clone + Display {

    pub fn new(size: Vec2<usize>, initializer: &T) -> Self {
        Self {
            size,
            data: vec![initializer.clone(); size.x * size.y]
        }
    }

    pub fn new_with_data(size: Vec2<usize>, data: &Vec<T>) -> Self {
        if !data.len() == (size.x * size.y) { panic!("Invalid data size"); }
        Self {
            size,
            data: data.clone()
        }
    }

    pub fn new_with_owned_data(size: Vec2<usize>, data: Vec<T>) -> Self {
        if !data.len() == (size.x * size.y) { panic!("Invalid data size"); }
        Self { size, data }
    }

    pub fn coord_to_ix(&self, coord: &Vec2<usize>) -> usize {
        coord.x + (coord.y * self.size.x)
    }

    pub fn coords_to_ix(&self, x: usize, y: usize) -> usize {
        x + (y * self.size.x)
    }

    pub fn ix_to_coord(&self, ix: usize) -> Vec2<usize> {
        Vec2::new(
            ix % self.size.x,
            ix / self.size.x
        )
    }

    pub fn get(&self, ix: usize) -> T {
        self.data[ix].clone()
    }

    pub fn get_at_coord(&self, coord: &Vec2<usize>) -> T {
        self.data[self.coord_to_ix(coord)].clone()
    }

    pub fn get_at_coords(&self, x: usize, y: usize) -> T {
        self.data[self.coords_to_ix(x, y)].clone()
    }

    pub fn set(&mut self, ix: usize, val: &T) {
        self.data[ix] = val.clone();
    }

    pub fn set_at_coord(&mut self, coord: &Vec2<usize>, val: &T) {
        let ix = self.coord_to_ix(coord);
        self.data[ix] = val.clone();
    }

    pub fn set_at_coords(&mut self, x: usize, y: usize, val: &T) {
        let ix = self.coords_to_ix(x, y);
        self.data[ix] = val.clone();
    }

    pub fn apply_at_coord(&mut self, coord: &Vec2<usize>, f: fn(&T) -> T) {
        let ix = self.coord_to_ix(coord);
        self.data[ix] = f(&self.data[ix]);
    }

    pub fn raw_data(&self) -> &Vec<T> {
        &self.data
    }

    pub fn get_size(&self) -> Vec2<usize> {
        self.size.clone()
    }

    pub fn get_element_count(&self) -> usize {
        self.size.x * self.size.y
    }

    pub fn is_on_left_edge(&self, ix: usize) -> bool {
        ix % self.size.x == 0
    }

    pub fn is_on_right_edge(&self, ix: usize) -> bool {
        self.is_on_left_edge(ix + 1)
    }

    pub fn is_on_top_edge(&self, ix: usize) -> bool {
        ix < self.size.x
    }

    pub fn is_on_bottom_edge(&self, ix: usize) -> bool {
        ix >= ((self.size.x * self.size.y) - self.size.x)
    }

    pub fn is_on_edge(&self, ix: usize) -> bool {
        self.is_on_left_edge(ix) || self.is_on_right_edge(ix) || self.is_on_top_edge(ix) || self.is_on_bottom_edge(ix)
    }

    pub fn get_left(&self, ix: usize) -> Option<usize> {
        if self.is_on_left_edge(ix) { None } else { Some(ix - 1) }
    }

    pub fn get_right(&self, ix: usize) -> Option<usize> {
        if self.is_on_right_edge(ix) { None } else { Some(ix + 1) }
    }

    pub fn get_up(&self, ix: usize) -> Option<usize> {
        if self.is_on_top_edge(ix) { None } else { Some(ix - self.size.x) }
    }

    pub fn get_down(&self, ix: usize) -> Option<usize> {
        if self.is_on_bottom_edge(ix) { None } else { Some(ix + self.size.x) }
    }

    pub fn get_up_left(&self, ix: usize) -> Option<usize> {
        self.get_up(ix).iter().flat_map(|&x| self.get_left(x)).next()
    }

    pub fn get_up_right(&self, ix: usize) -> Option<usize> {
        self.get_up(ix).iter().flat_map(|&x| self.get_right(x)).next()
    }

    pub fn get_down_left(&self, ix: usize) -> Option<usize> {
        self.get_down(ix).iter().flat_map(|&x| self.get_left(x)).next()
    }

    pub fn get_down_right(&self, ix: usize) -> Option<usize> {
        self.get_down(ix).iter().flat_map(|&x| self.get_right(x)).next()
    }

    pub fn get_left_value(&self, ix: usize) -> Option<T> { self.get_left(ix).map(|adj| self.get(adj)) }
    pub fn get_right_value(&self, ix: usize) -> Option<T> { self.get_right(ix).map(|adj| self.get(adj)) }
    pub fn get_up_value(&self, ix: usize) -> Option<T> { self.get_up(ix).map(|adj| self.get(adj)) }
    pub fn get_down_value(&self, ix: usize) -> Option<T> { self.get_down(ix).map(|adj| self.get(adj)) }

    pub fn is_coord_on_left_edge(&self, coord: &Vec2<usize>) -> bool {
        coord.x == 0
    }

    pub fn is_coord_on_right_edge(&self, coord: &Vec2<usize>) -> bool {
        coord.x == (self.size.x - 1)
    }

    pub fn is_coord_on_top_edge(&self, coord: &Vec2<usize>) -> bool {
        coord.y == 0
    }

    pub fn is_coord_on_bottom_edge(&self, coord: &Vec2<usize>) -> bool {
        coord.y == (self.size.y - 1)
    }

    pub fn get_coord_left_value(&self, coord: &Vec2<usize>) -> Option<T> {
        self.get_left_value(self.coord_to_ix(coord))
    }
    pub fn get_coord_right_value(&self, coord: &Vec2<usize>) -> Option<T> {
        self.get_right_value(self.coord_to_ix(coord))
    }
    pub fn get_coord_up_value(&self, coord: &Vec2<usize>) -> Option<T> {
        self.get_up_value(self.coord_to_ix(coord))
    }
    pub fn get_coord_down_value(&self, coord: &Vec2<usize>) -> Option<T> {
        self.get_down_value(self.coord_to_ix(coord))
    }

    pub fn get_surrounding(&self, ix: usize) -> Vec<usize> {
        [self.get_up(ix), self.get_right(ix), self.get_down(ix), self.get_left(ix)]
            .iter()
            .flat_map(|s| *s)
            .collect_vec()
    }

    pub fn get_surrounding_incl_diagonals(&self, ix: usize) -> Vec<usize> {
        [self.get_up(ix), self.get_up_right(ix), self.get_right(ix), self.get_down_right(ix),
         self.get_down(ix), self.get_down_left(ix), self.get_left(ix), self.get_up_left(ix)]
            .iter()
            .flat_map(|s| *s)
            .collect_vec()
    }

    pub fn get_adjacent_to_region(&self, region_start: &Vec2<usize>, region_end: &Vec2<usize>, allow_diagonals: bool) -> Vec<usize> {
        let (min, max) = (
            Vec2::new(region_start.x.min(region_end.x), region_start.y.min(region_end.y)),
            Vec2::new(region_start.x.max(region_end.x), region_start.y.max(region_end.y)));

        let mut result = Vec::new();

        (min.x..=max.x).map(|x| self.coords_to_ix(x, min.y))    // Top
            .filter_map(|ix| self.get_up(ix))
            .for_each(|ix| result.push(ix));

        (min.x..=max.x).map(|x| self.coords_to_ix(x, max.y))    // Bottom
            .filter_map(|ix| self.get_down(ix))
            .for_each(|ix| result.push(ix));

        (min.y..=max.y).map(|y| self.coords_to_ix(min.x, y))    // Left
            .filter_map(|ix| self.get_left(ix))
            .for_each(|ix| result.push(ix));

        (min.y..=max.y).map(|y| self.coords_to_ix(max.x, y))    // Right
            .filter_map(|ix| self.get_right(ix))
            .for_each(|ix| result.push(ix));

        if allow_diagonals {
            self.get_up_left(self.coords_to_ix(min.x, min.y)).iter().for_each(|&ix| result.push(ix));      // Up-left
            self.get_up_right(self.coords_to_ix(max.x, min.y)).iter().for_each(|&ix| result.push(ix));     // Down-left
            self.get_down_left(self.coords_to_ix(min.x, max.y)).iter().for_each(|&ix| result.push(ix));    // Up-right
            self.get_down_right(self.coords_to_ix(max.x, max.y)).iter().for_each(|&ix| result.push(ix));   // Down-right
        }

        result
    }

    pub fn to_string(&self) -> String {
        self.data.iter()
            .chunks(self.size.x).into_iter().map(|chunk| chunk
                .map(|x| x.to_string()).join(""))
            .join("\n")
    }
}