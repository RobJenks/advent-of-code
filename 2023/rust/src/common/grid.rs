use std::fmt::{Display, Formatter};
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

    pub fn new_from_2d_data(data: &Vec<Vec<T>>) -> Self {
        Self {
            size: Vec2::new(data.iter().next().unwrap_or_else(|| panic!("No grid data")).len(), data.len()),
            data: data.iter().flat_map(|v| v.iter().cloned()).collect_vec()
        }
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

    pub fn get_ref(&self, ix: usize) -> &T {
        &self.data[ix]
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

    pub fn is_valid_coord(&self, coord: &Vec2<usize>) -> bool {
        coord.x < self.size.x && coord.y < self.size.y
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

    pub fn get_in_direction(&self, ix: usize, dir: GridDirection) -> Option<usize> {
        match dir {
            GridDirection::Left => self.get_left(ix),
            GridDirection::Up => self.get_up(ix),
            GridDirection::Right => self.get_right(ix),
            GridDirection::Down => self.get_down(ix)
        }
    }

    pub fn get_left_value(&self, ix: usize) -> Option<T> { self.get_left(ix).map(|adj| self.get(adj)) }
    pub fn get_right_value(&self, ix: usize) -> Option<T> { self.get_right(ix).map(|adj| self.get(adj)) }
    pub fn get_up_value(&self, ix: usize) -> Option<T> { self.get_up(ix).map(|adj| self.get(adj)) }
    pub fn get_down_value(&self, ix: usize) -> Option<T> { self.get_down(ix).map(|adj| self.get(adj)) }

    pub fn get_value_in_direction(&self, ix: usize, dir: GridDirection) -> Option<T> {
        self.get_in_direction(ix, dir).map(|adj| self.get(adj))
    }

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

    pub fn get_row(&self, row: usize) -> Option<Vec<T>> {
        Some(row)
            .filter(|&r| r < self.size.y)
            .map(|r| self.data[r*self.size.x..(r+1)*self.size.x].iter().cloned().collect_vec())
    }

    pub fn get_col(&self, col: usize) -> Option<Vec<T>> {
        Some(col)
            .filter(|&c| c < self.size.x)
            .map(|c| (0..self.size.y).map(|r| self.get(c + (r * self.size.x))).collect_vec())
    }

    pub fn get_row_index(&self, ix: usize) -> usize {
        ix / self.size.x
    }

    pub fn get_col_index(&self, ix: usize) -> usize {
        ix % self.size.x
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

    pub fn manhattan_dist(&self, from_cell: usize, to_cell: usize) -> usize {
        let from_pos = self.ix_to_coord(from_cell);
        let to_pos = self.ix_to_coord(to_cell);
        ((to_pos.x as isize - from_pos.x as isize).abs() + (to_pos.y as isize - from_pos.y as isize).abs()) as usize
    }

    // Returns the grid direction from->to, IF there is a straight line between the two elements, AND they are not the same
    pub fn straight_direction_from(&self, from: usize, to: usize) -> Option<GridDirection> {
        if from == to { return None }

        let from_pos = self.ix_to_coord(from);
        let to_pos = self.ix_to_coord(to);
        if from_pos.x == to_pos.x {             // In the same column
            if from_pos.y < to_pos.y {
                Some(GridDirection::Down)
            }
            else {
                Some(GridDirection::Up)
            }
        }
        else if from_pos.y == to_pos.y {        // In the same row
            if from_pos.x < to_pos.x {
                Some(GridDirection::Right)
            }
            else {
                Some(GridDirection::Left)
            }
        }
        else {
            None
        }
    }

    pub fn get_cells_in_direction_until(&self, from: usize, dir: GridDirection,
                                        stop_fn: impl Fn(/* current_cell:*/usize, /*num_taken:*/usize) -> bool) -> Vec<usize> {
        let mut cells = Vec::new();
        let mut current = from;

        for num_taken in 1usize.. {
            if let Some(next_cell) = self.get_in_direction(current, dir) {
                if stop_fn(next_cell, num_taken) { break}

                cells.push(next_cell);
                current = next_cell;
            }
            else { break }
        }

        cells
    }

    pub fn get_n_cells_in_direction(&self, from: usize, dir: GridDirection, num_cells: usize) -> Vec<usize> {
        self.get_cells_in_direction_until(from, dir, |_, num_taken| num_taken > num_cells)
    }

    pub fn get_nth_cell_left_from_coord(&self, coord: &Vec2<usize>, n: usize) -> Option<Vec2<usize>> {
        if coord.x < n {
            None
        }
        else {
            Some(Vec2::new(coord.x - n, coord.y))
        }
    }

    pub fn get_nth_cell_up_from_coord(&self, coord: &Vec2<usize>, n: usize) -> Option<Vec2<usize>> {
        if coord.y < n {
            None
        }
        else {
            Some(Vec2::new(coord.x, coord.y - n))
        }
    }

    pub fn get_nth_cell_right_from_coord(&self, coord: &Vec2<usize>, n: usize) -> Option<Vec2<usize>> {
        if n >= (self.size.x - coord.x) {
            None
        }
        else {
            Some(Vec2::new(coord.x + n, coord.y))
        }
    }

    pub fn get_nth_cell_down_from_coord(&self, coord: &Vec2<usize>, n: usize) -> Option<Vec2<usize>> {
        if n > (self.size.y - coord.y) {
            None
        }
        else {
            Some(Vec2::new(coord.x, coord.y + n))
        }
    }

    pub fn get_nth_cell_in_direction_from_coord(&self, coord: &Vec2<usize>, dir: GridDirection, n: usize) -> Option<Vec2<usize>> {
        match dir {
            GridDirection::Left => self.get_nth_cell_left_from_coord(coord, n),
            GridDirection::Up => self.get_nth_cell_up_from_coord(coord, n),
            GridDirection::Right => self.get_nth_cell_right_from_coord(coord, n),
            GridDirection::Down => self.get_nth_cell_down_from_coord(coord, n)
        }
    }

    pub fn flood_fill(&mut self, start: usize, fill_action: fn(&mut T), can_flood: fn(&Grid<T>, usize, usize, &T) -> bool) {
        let element_count = self.get_element_count();
        let mut tested = vec![false; element_count];
        let mut is_head = vec![false; element_count];   // Indexed version of `head` list for lookup efficiency

        let mut head = vec![(start, start)];    // Vec of (cell which flooded into head cell, head cell)
        is_head[start] = true;

        while let Some((prev, next)) = head.pop() {
            tested[next] = true;
            let value = self.get_ref(next);

            if !can_flood(self, prev, next, value) {
                continue;
            }

            fill_action(&mut self.data[next]);

            self.get_surrounding(next).into_iter()
                .filter(|adj| !tested[*adj])
                .for_each(|adj| if !is_head[adj] {
                    is_head[adj] = true;
                    head.push((next, adj))
                });
        }
    }

    pub fn to_string(&self) -> String {
        self.data.iter()
            .chunks(self.size.x).into_iter().map(|chunk| chunk
                .map(|x| x.to_string()).join(""))
            .join("\n")
    }
}

impl <T> Clone for Grid<T>
    where T: Clone + Display {

    fn clone(&self) -> Self {
        Self { size: self.size, data: self.data.clone() }
    }
}

#[repr(u32)]
#[derive(Eq, PartialEq, Copy, Clone, Debug, Hash)]
pub enum GridDirection {
    Left = 0,
    Up = 1,
    Right = 2,
    Down = 3
}

const OPPOSITE_GRID_DIRECTIONS : [GridDirection; 4] = [
    GridDirection::Right,    // Opposite of Left (= 0)
    GridDirection::Down,     // Opposite of Up (= 1)
    GridDirection::Left,     // Opposite of Right (= 2)
    GridDirection::Up,       // Opposite of Down (= 3)
];

const DIRECTION_UNIT_MOVEMENTS : [Vec2<isize>; 4] = [
    Vec2 { x: -1, y: 0 },    // Left (= 0)
    Vec2 { x: 0, y: -1 },    // Up (= 1)
    Vec2 { x: 1, y: 0 },     // Right (= 2)
    Vec2 { x: 0, y: 1 }      // Down (= 3)
];

impl GridDirection {
    pub fn unit_movement(&self) -> Vec2<isize> {
        DIRECTION_UNIT_MOVEMENTS[*self as usize]
    }

    pub fn opposite(&self) -> GridDirection {
        OPPOSITE_GRID_DIRECTIONS[*self as usize]
    }

    pub fn directions() -> [GridDirection; 4] {
        [GridDirection::Left, GridDirection::Up, GridDirection::Down, GridDirection::Right]
    }
}

impl Display for GridDirection {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}