use crate::common::vec2::Vec2;
use enum_primitive::FromPrimitive;
use std::slice::Iter;
use itertools::Itertools;
use std::fmt::Display;

const COORD_OFFSETS : [(isize,isize); 8] = [(0,-1), (1, -1), (1,0), (1,1), (0,1), (-1,1), (-1,0), (-1,-1)];

#[derive(Clone, Debug)]
pub struct Grid<T>
    where T: Default + Clone + Display {

    data: Vec<T>,
    size: Vec2<usize>,
    count: usize,

    ix_offsets: [isize; 8]
}

impl <T> Grid<T>
    where T: Default + Clone + Display {

    pub fn new(nx: usize, ny: usize) -> Self {
        let count = nx * ny;
        Self {
            data: vec![T::default(); count],
            size: Vec2::new(nx, ny),
            count,
            ix_offsets: Self::build_ix_offsets(nx, ny)
        }
    }

    pub fn new_from_data(data: &Vec<Vec<T>>) -> Self {
        assert_ne!(0, data.len());
        assert_ne!(0, data[0].len());
        assert_eq!(1, data.iter().map(|v| v.len()).unique().count());

        let mut grid = Grid::<T>::new(data[0].len(), data.len());
        data.iter().enumerate().for_each(|(y,row)| {
            row.iter().enumerate().for_each(|(x, val)| grid.set_at_coords(x, y, val.clone()));
        });

        grid
    }

    pub fn index(&self, x: usize, y: usize) -> usize {
        (y * self.size.x) + x
    }

    pub fn index_coord(&self, coord: &Vec2<usize>) -> usize {
        self.index(coord.x, coord.y)
    }

    pub fn try_index(&self, x: usize, y: usize) -> Option<usize>{
        if self.valid_coords(x, y) {
            Some(self.index(x, y))
        }
        else { None }
    }

    pub fn try_index_coord(&self, coord: &Vec2<usize>) -> Option<usize>{
        self.try_index(coord.x, coord.y)
    }

    pub fn coords(&self, index: usize) -> Vec2<usize> {
        Vec2::new(index % self.size.x, index / self.size.x)
    }

    pub fn try_coords(&self, index: usize) -> Option<Vec2<usize>> {
        if self.valid_index(index) {
            Some(self.coords(index))
        }
        else { None }
    }

    pub fn valid_index(&self, index: usize) -> bool {
        index < self.count
    }

    pub fn valid_coord(&self, coord: &Vec2<usize>) -> bool {
        self.valid_coords(coord.x, coord.y)
    }

    pub fn valid_coords(&self, x: usize, y: usize) -> bool {
        x < self.size.x && y < self.size.y
    }

    pub fn valid_signed_coords(&self, x: isize, y: isize) -> bool {
        x >= 0 && y >= 0 && x < self.size.x as isize && y < self.size.y as isize
    }

    pub fn get_data(&self) -> &Vec<T> { &self.data }
    pub fn get_data_mut(&mut self) -> &mut Vec<T> { &mut self.data }
    pub fn get_size(&self) -> Vec2<usize> { self.size }
    pub fn get_count(&self) -> usize { self.count }

    pub fn get(&self, index: usize) -> &T { &self.data[index] }
    pub fn get_at_coord(&self, coord: &Vec2<usize>) -> &T { &self.data[self.index_coord(coord)] }

    pub fn get_mut(&mut self, index: usize) -> &mut T { &mut self.data[index] }
    pub fn get_mut_at_coord(&mut self, coord: &Vec2<usize>) -> &mut T {
        let ix = self.index_coord(coord);
        &mut self.data[ix]
    }

    pub fn try_get(&self, index: usize) -> Option<&T> {
        if self.valid_index(index) { Some(self.get(index)) } else { None }
    }
    pub fn try_get_at_coord(&self, coord: &Vec2<usize>) -> Option<&T> {
        self.try_index_coord(coord).map(|i| self.get(i))
    }

    pub fn try_get_mut(&mut self, index: usize) -> Option<&mut T> {
        if self.valid_index(index) { Some(self.get_mut(index)) } else { None }
    }
    pub fn try_get_mut_at_coord(&mut self, coord: &Vec2<usize>) -> Option<&mut T> {
        self.try_index_coord(coord).map(move |i| self.get_mut(i))
    }

    pub fn set(&mut self, index: usize, val: T) { self.data[index] = val; }
    pub fn set_at_coord(&mut self, coord: &Vec2<usize>, val: T) {
        let ix = self.index_coord(coord);
        self.data[ix] = val;
    }
    pub fn set_at_coords(&mut self, x: usize, y: usize, val: T) {
        let ix = self.index(x, y);
        self.data[ix] = val;
    }

    pub fn try_set(&mut self, index: usize, val: T) {
        if self.valid_index(index) { self.set(index, val) }
    }
    pub fn try_set_at_coord(&mut self, coord: &Vec2<usize>, val: T) {
        self.try_index_coord(coord).map(|i| self.set(i, val));
    }

    pub fn is_edge_index(&self, index: usize) -> bool {
        self.is_edge_coord(&self.coords(index))
    }
    pub fn is_edge_coord(&self, coord: &Vec2<usize>) -> bool {
        coord.x == 0 || coord.x == self.size.x-1 ||
        coord.y == 0 || coord.y == self.size.y-1
    }

    fn build_ix_offsets(width: usize, height: usize) -> [isize; 8] {
        let (nx, ny) = (width as isize, height as isize);
        [-nx, -nx+1, 1, nx+1, nx, nx-1, -1, -nx-1]
    }

    pub fn get_all_neighbours_unchecked(&self, ix: usize) -> [&T;8] {
        let index = ix as isize;
        [ &self.data[(index + self.ix_offsets[0]) as usize],
          &self.data[(index + self.ix_offsets[1]) as usize],
          &self.data[(index + self.ix_offsets[2]) as usize],
          &self.data[(index + self.ix_offsets[3]) as usize],
          &self.data[(index + self.ix_offsets[4]) as usize],
          &self.data[(index + self.ix_offsets[5]) as usize],
          &self.data[(index + self.ix_offsets[6]) as usize],
          &self.data[(index + self.ix_offsets[7]) as usize]
        ]
    }

    pub fn get_all_coord_neighbours_unchecked(&self, coord: &Vec2<usize>) -> [&T;8] {
        self.get_all_neighbours_unchecked(self.index_coord(coord))
    }

    pub fn get_all_neighbours(&self, ix: usize) -> Vec<&T> {
        self.get_all_coord_neighbours(&self.coords(ix))
    }

    pub fn get_all_coord_neighbours(&self, coord: &Vec2<usize>) -> Vec<&T> {
        let (ix, iy) = (coord.x as isize, coord.y as isize);
        COORD_OFFSETS.iter()
            .map(|(ox, oy)| (ix + ox, iy + oy))
            .filter(|(x,y)| self.valid_signed_coords(*x, *y))
            .map(|(x, y)| self.get(self.index(x as usize, y as usize)))
            .collect()
    }

    pub fn get_edge_indices(&self) -> Vec<usize> {
        (0..self.count).filter(|&i| self.is_edge_index(i)).collect()
    }

    pub fn get_non_edge_indices(&self) -> Vec<usize> {
        (0..self.count).filter(|&i| !self.is_edge_index(i)).collect()
    }

    pub fn print_data(&self) -> String {
        self.data.chunks(self.size.x)
            .map(|line| line.iter().map(|x| format!("{}", x)).join(""))
            .join("\n")
    }
}

enum_from_primitive! {
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum GridDir {
    Up = 0,
    UpRight = 1,
    Right = 2,
    DownRight = 3,
    Down = 4,
    DownLeft = 5,
    Left = 6,
    UpLeft = 7
}
}

impl From<usize> for GridDir {
    fn from(val: usize) -> Self {
        GridDir::from_usize(val).expect(format!("Invalid value ({}) passed as grid direction", val).as_str())
    }
}

impl From<GridDir> for usize {
    fn from(dir: GridDir) -> Self {
        dir as usize
    }
}

#[cfg(test)]
mod tests {
    use crate::common::grid::Grid;
    use crate::common::vec2::Vec2;
    use itertools::Itertools;

    #[test]
    pub fn test_basic_setup() {
        let grid = Grid::<usize>::new(5, 5);

        assert_eq!(Vec2::new(5, 5), grid.get_size());
        assert_eq!(25, grid.get_count());
        assert_eq!(&vec![usize::default(); 25], grid.get_data());
    }

    #[test]
    pub fn test_indexing() {
        let grid = Grid::<usize>::new(5, 5);

        assert_eq!(0, grid.index_coord(&Vec2::new(0, 0)));
        assert_eq!(4, grid.index_coord(&Vec2::new(4, 0)));
        assert_eq!(5, grid.index_coord(&Vec2::new(0, 1)));
        assert_eq!(24, grid.index_coord(&Vec2::new(4, 4)));

        assert_eq!(Some(0), grid.try_index_coord(&Vec2::new(0, 0)));
        assert_eq!(Some(4), grid.try_index_coord(&Vec2::new(4, 0)));
        assert_eq!(Some(5), grid.try_index_coord(&Vec2::new(0, 1)));
        assert_eq!(Some(24), grid.try_index_coord(&Vec2::new(4, 4)));

        assert_eq!(None, grid.try_index_coord(&Vec2::new(5, 2)));
        assert_eq!(None, grid.try_index_coord(&Vec2::new(2, 5)));
        assert_eq!(None, grid.try_index_coord(&Vec2::new(5, 5)));
        assert_eq!(None, grid.try_index_coord(&Vec2::new(12, 12)));
    }

    #[test]
    pub fn test_coords() {
        let grid = Grid::<usize>::new(5, 5);

        assert_eq!(Vec2::new(0, 0), grid.coords(0));
        assert_eq!(Vec2::new(4, 0), grid.coords(4));
        assert_eq!(Vec2::new(0, 1), grid.coords(5));
        assert_eq!(Vec2::new(4, 4), grid.coords(24));

        assert_eq!(Some(Vec2::new(0, 0)), grid.try_coords(0));
        assert_eq!(Some(Vec2::new(4, 0)), grid.try_coords(4));
        assert_eq!(Some(Vec2::new(0, 1)), grid.try_coords(5));
        assert_eq!(Some(Vec2::new(4, 4)), grid.try_coords(24));

        assert_eq!(None, grid.try_coords(25));
        assert_eq!(None, grid.try_coords(100));
    }

    #[test]
    pub fn test_edges() {
        let grid = Grid::<usize>::new(4, 4);

        assert_eq!(vec![0,1,2,3,4,7,8,11,12,13,14,15], grid.get_edge_indices());
        assert_eq!(vec![5,6,9,10], grid.get_non_edge_indices());
    }

    #[test]
    pub fn test_neighbours() {
        let mut grid = Grid::<usize>::new(4, 4);
        (0..15).for_each(|i| grid.set(i, i));

        // Inner element
        assert_eq!(vec![0,1,2,4,6,8,9,10], grid.get_all_neighbours(5).iter().sorted().map(|x| **x).collect::<Vec<usize>>());
        assert_eq!(vec![0,1,2,4,6,8,9,10], grid.get_all_neighbours_unchecked(5).iter().sorted().map(|x| **x).collect::<Vec<usize>>());

        // Edge element
        assert_eq!(vec![2,6,7], grid.get_all_neighbours(3).iter().sorted().map(|x| **x).collect::<Vec<usize>>());
    }
}
