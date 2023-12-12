use itertools::Itertools;
use crate::common::grid::Grid;
use crate::common::vec2::Vec2;

pub struct Maze {
    pub grid : Grid<char>,
    pub size : Vec2<usize>
}

impl Maze {
    pub fn new(size: Vec2<usize>, data: Vec<char>) -> Self {
        Self {
            size,
            grid: Grid::new_with_owned_data(size, data)
        }
    }

    pub fn find_start(&self) -> usize {
        self.grid.raw_data().iter()
            .find_position(|&c| *c == 'S')
            .map(|(ix, _)| ix)
            .unwrap_or_else(|| panic!("Cannot locate start"))
    }

    pub fn find_connections_into_cell(&self, cell: usize) -> Vec<usize> {
        let surrounding = self.grid.get_surrounding(cell);
        let connected = surrounding.iter()
            .filter(|&ix| self.get_connections(*ix).contains(&cell))
            .cloned()
            .collect_vec();

        assert_eq!(connected.len(), 2);
        connected
    }

    pub fn get_connections(&self, cell: usize) -> Vec<usize> {
        match self.grid.get(cell) {
            '|' => vec![self.grid.get_up(cell).unwrap(), self.grid.get_down(cell).unwrap()],
            '-' => vec![self.grid.get_left(cell).unwrap(), self.grid.get_right(cell).unwrap()],
            'L' => vec![self.grid.get_up(cell).unwrap(), self.grid.get_right(cell).unwrap()],
            'J' => vec![self.grid.get_up(cell).unwrap(), self.grid.get_left(cell).unwrap()],
            '7' => vec![self.grid.get_left(cell).unwrap(), self.grid.get_down(cell).unwrap()],
            'F' => vec![self.grid.get_right(cell).unwrap(), self.grid.get_down(cell).unwrap()],
            '.' => vec![],
            'S' => panic!("Unknown connections from starting cell"),
            x @ _ => panic!("Invalid cell value '{}'", x)
        }
    }

    pub fn get_valid_connections(&self, cell: usize) -> [usize; 2] {
        match self.grid.get(cell) {
            '|' => [self.grid.get_up(cell).unwrap(), self.grid.get_down(cell).unwrap()],
            '-' => [self.grid.get_left(cell).unwrap(), self.grid.get_right(cell).unwrap()],
            'L' => [self.grid.get_up(cell).unwrap(), self.grid.get_right(cell).unwrap()],
            'J' => [self.grid.get_up(cell).unwrap(), self.grid.get_left(cell).unwrap()],
            '7' => [self.grid.get_left(cell).unwrap(), self.grid.get_down(cell).unwrap()],
            'F' => [self.grid.get_right(cell).unwrap(), self.grid.get_down(cell).unwrap()],
            x @ _ => panic!("Invalid cell value '{}'", x)
        }
    }

    pub fn get_next(&self, cell: usize, prev_cell: usize) -> usize {
        let connected = self.get_valid_connections(cell);
        if connected[0] == prev_cell {
            connected[1]
        }
        else {
            assert_eq!(connected[1], prev_cell);
            connected[0]
        }
    }
}