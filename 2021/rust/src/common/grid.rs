use crate::common::vec2::Vec2;

pub struct Grid<T>
    where T: Clone {

    size: Vec2<usize>,
    data: Vec<T>
}

#[allow(dead_code)]
impl <T> Grid<T>
    where T: Clone {

    pub fn new(size: Vec2<usize>, initializer: &T) -> Self {
        Self {
            size,
            data: vec![initializer.clone(); size.x * size.y]
        }
    }

    fn coord_to_ix(&self, coord: &Vec2<usize>) -> usize {
        coord.x + (coord.y * self.size.x)
    }

    fn coords_to_ix(&self, x: usize, y: usize) -> usize {
        x + (y * self.size.x)
    }

    fn ix_to_coord(&self, ix: usize) -> Vec2<usize> {
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

}