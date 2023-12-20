use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;
use std::iter::Iterator;
use itertools::Itertools;
use crate::common::grid::{Grid, GridDirection};
use crate::common::pathfinding;
use super::common;

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> usize {
    find_path(&parse_input("src/day17/problem-input.txt"), 1, 3).total_cost
}

fn part2() -> usize {
    find_path(&parse_input("src/day17/problem-input.txt"), 4, 10).total_cost
}

fn find_path(grid: &Grid<u32>, min_travel_dist: usize, max_travel_dist: usize) -> PathResult {
    let nodes = build_node_graph(grid, min_travel_dist, max_travel_dist);

    let start = NodeRef::new(0, GridDirection::Up);
    let end = vec![
        NodeRef::new(grid.get_element_count() - 1, GridDirection::Right),
        NodeRef::new(grid.get_element_count() - 1, GridDirection::Down)];

    let (path, cost) = pathfinding::astar::find_path(start, &end,
                 // get_connected:
                 |id| nodes.get(id).unwrap().connections.iter().map(|conn| conn.target.clone()).collect_vec(),

                 // get_cost:
                 |from, to| nodes.get(from).unwrap_or_else(|| panic!("Missing source node"))
                     .find_connection(to).unwrap_or_else(|| panic!("Missing connection")).cost)

        .unwrap_or_else(|| panic!("No solution"));

    //PathResult::new(path.iter().map(|n| n.pos).collect_vec(), path_cost(&nodes, &path))
    PathResult::new(path.iter().map(|n| n.pos).collect_vec(), cost as usize)
}

fn build_node_graph(grid: &Grid<u32>, min_conn_dist: usize, max_conn_dist: usize) -> HashMap<NodeRef, NodeData> {
    let el_count = grid.get_element_count();
    let dirs = GridDirection::directions();

    let mut nodes = (0..el_count).cartesian_product(dirs.iter())
            .map(|(el, dir)| (NodeRef::new(el, *dir), NodeData::new()))
            .collect::<HashMap<NodeRef, NodeData>>();

    (0..el_count).cartesian_product(dirs.iter()).for_each(|(el, dir)| {
        let node = NodeRef::new(el, dir.clone());
        let allowable_outbound_dirs = &OUTBOUND_DIRS[*dir as usize];
        add_nodes_in_dir(grid, &mut nodes, &node, allowable_outbound_dirs[0], min_conn_dist, max_conn_dist);
        add_nodes_in_dir(grid, &mut nodes, &node, allowable_outbound_dirs[1], min_conn_dist, max_conn_dist);
    });

    nodes
}

const OUTBOUND_DIRS: [[GridDirection; 2]; 4] = [
    [GridDirection::Up, GridDirection::Down],        // Currently going Left(==0), so can now go Up or Down
    [GridDirection::Left, GridDirection::Right],     // Currently going Up(==1), so can now go Left or Right
    [GridDirection::Up, GridDirection::Down],        // Currently going Right(==2), so can now go Up or Down
    [GridDirection::Left, GridDirection::Right]      // Currently going Down(==3), so can now go Left or Right
];


fn add_nodes_in_dir(grid: &Grid<u32>, nodes: &mut HashMap<NodeRef, NodeData>, from_node: &NodeRef, get_nodes_in_dir: GridDirection,
                    min_dist: usize, max_dist: usize) {
    let current_node = nodes.get_mut(from_node).unwrap_or_else(|| panic!("Missing node"));
    let current_coord = grid.ix_to_coord(from_node.pos);

    let cells_before_start = grid.get_n_cells_in_direction(from_node.pos, get_nodes_in_dir, min_dist - 1);
    let mut total_cost = cells_before_start.iter().map(|c| grid.get(*c) as isize).sum();

    let start_cell = grid.get_nth_cell_in_direction_from_coord(&current_coord, get_nodes_in_dir, min_dist - 1);
    if start_cell.is_none() { return }
    let start_ix = grid.coord_to_ix(&start_cell.unwrap());
    let num_to_fetch = max_dist - min_dist + 1;

    let adj_cells = grid.get_n_cells_in_direction(start_ix, get_nodes_in_dir, num_to_fetch);

    for adj in adj_cells {
        total_cost += grid.get(adj) as isize;
        current_node.connections.push(NodeConnection::new(NodeRef::new(adj, get_nodes_in_dir), total_cost));
    }
}

const NO_NODE: usize = usize::MAX;

// NodeRef

#[derive(Eq, Clone, Hash, Debug)]
pub struct NodeRef {
    pos: usize,
    travel_dir: GridDirection      // Direction that we travelled to reach this node
}
impl NodeRef {
    pub fn new(pos: usize, travel_dir: GridDirection) -> Self {
        Self { pos, travel_dir }
    }
}

impl pathfinding::astar::NodeId for NodeRef {
    fn none() -> Self {
        Self { pos: NO_NODE, travel_dir: GridDirection::Left }
    }

    fn is_none(&self) -> bool {
        self.pos == NO_NODE
    }
}

impl PartialEq for NodeRef {
    fn eq(&self, other: &Self) -> bool {
        self.pos == other.pos && self.travel_dir == other.travel_dir
    }
}

impl Display for NodeRef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}({})", self.pos, self.travel_dir)
    }
}

// NodeData

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
struct NodeData {
    pub connections: Vec<NodeConnection>
}
impl NodeData {
    pub fn new() -> Self {
        Self { connections: Vec::new() }
    }
    pub fn find_connection(&self, target: &NodeRef) -> Option<&NodeConnection> {
        self.connections.iter().find(|&conn| target == &conn.target)
    }
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
struct NodeConnection {
    pub target: NodeRef,
    pub cost: isize
}
impl NodeConnection {
    pub fn new(target: NodeRef, cost: isize) -> Self {
        Self { target, cost }
    }
}

// PathResult

struct PathResult {
    #[allow(unused)]
    nodes: Vec<usize>,
    total_cost: usize
}
impl PathResult {
    pub fn new(nodes: Vec<usize>, total_cost: usize) -> Self {
        Self { nodes, total_cost }
    }
}

fn parse_input(file: &str) -> Grid<u32> {
    Grid::new_from_2d_data(
        &common::read_file(file)
            .lines()
            .map(|s| s.trim().chars()
                .map(|c| c.to_digit(10).unwrap_or_else(|| panic!("Invalid input value '{}'", c)))
                .collect_vec())
            .collect_vec())
}


#[cfg(test)]
mod tests {
    use crate::day17::{find_path, parse_input, part1, part2};

    #[test]
    fn test_basic_pathfinding() {
        let result = find_path(&parse_input("src/day17/test-input-1.txt"), 1, 3);
        assert_eq!((result.nodes, result.total_cost), (vec![0, 2, 8, 11], 7));
    }

    #[test]
    fn test_constrained_cost_pathfinding() {
        let result = find_path(&parse_input("src/day17/test-input-2.txt"), 1, 3);
        assert_eq!(result.total_cost, 102);
    }

    #[test]
    fn test_complex_connection_pathfinding() {
        let result = find_path(&parse_input("src/day17/test-input-3.txt"), 4, 10);
        assert_eq!(result.total_cost, 71);
    }

    #[test]
    fn test_part1() {
        assert_eq!(part1(), 1076);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(), 1219);
    }

}
