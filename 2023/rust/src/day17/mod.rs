use std::cmp::Ordering;
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::hash::{Hash, Hasher};
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
    let result = find_path(&parse_input("src/day17/problem-input.txt"));
    println!("{:?}", result.nodes);
    result.total_cost
}

fn part2() -> usize {
    12
}

fn find_path(grid: &Grid<u32>) -> PathResult {
    let nodes = build_node_graph(grid);

    let start = NodeRef::new(0, GridDirection::Up);
    let end = vec![
        NodeRef::new(grid.get_element_count() - 1, GridDirection::Right),
        NodeRef::new(grid.get_element_count() - 1, GridDirection::Down)];

    let path = pathfinding::astar::find_path(start, &end,
                 // get_connected:
                 |id| nodes.get(id).unwrap().connections.iter().map(|conn| conn.target.clone()).collect_vec(),

                 // get_cost:
                 |from, to| nodes.get(from).unwrap_or_else(|| panic!("Missing source node"))
                     .find_connection(to).unwrap_or_else(|| panic!("Missing connection")).cost)

        .unwrap_or_else(|| panic!("No solution"));

    PathResult::new(path.iter().map(|n| n.pos).collect_vec(), path_cost(&nodes, &path))
}

fn build_node_graph(grid: &Grid<u32>) -> HashMap<NodeRef, NodeData> {
    let el_count = grid.get_element_count();
    let dirs = GridDirection::directions();

    let mut nodes = (0..el_count).cartesian_product(dirs.iter())
            .map(|(el, dir)| (NodeRef::new(el, *dir), NodeData::new(grid.get(el) as isize)))
            .collect::<HashMap<NodeRef, NodeData>>();

    (0..el_count).cartesian_product(dirs.iter()).for_each(|(el, dir)| {
        let node = NodeRef::new(el, dir.clone());
        let allowable_outbound_dirs = &OUTBOUND_DIRS[*dir as usize];
        add_nodes_in_dir(grid, &mut nodes, &node, allowable_outbound_dirs[0], 3);
        add_nodes_in_dir(grid, &mut nodes, &node, allowable_outbound_dirs[1], 3);
    });

    nodes
}

fn path_cost(nodes: &HashMap<NodeRef, NodeData>, path: &Vec<NodeRef>) -> usize {
    path.windows(2)
        .map(|segment| nodes.get(&segment[0]).unwrap().find_connection(&segment[1]).unwrap())
        .map(|conn| conn.cost as usize)
        .sum()
}


const OUTBOUND_DIRS: [[GridDirection; 2]; 4] = [
    [GridDirection::Up, GridDirection::Down],        // Currently going Left(==0), so can now go Up or Down
    [GridDirection::Left, GridDirection::Right],     // Currently going Up(==1), so can now go Left or Right
    [GridDirection::Up, GridDirection::Down],        // Currently going Right(==2), so can now go Up or Down
    [GridDirection::Left, GridDirection::Right]      // Currently going Down(==3), so can now go Left or Right
];


fn add_nodes_in_dir(grid: &Grid<u32>, nodes: &mut HashMap<NodeRef, NodeData>, from_node: &NodeRef, get_nodes_in_dir: GridDirection, num: usize) {
    let current_node = nodes.get_mut(from_node).unwrap_or_else(|| panic!("Missing node"));

    let adj_cells = grid.get_n_cells_in_direction(from_node.pos, get_nodes_in_dir, num);
    let mut total_cost = 0isize;

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
    pub fn new(cost: isize) -> Self {
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
    nodes: Vec<usize>,
    total_cost: usize
}
impl PathResult {
    pub fn new(nodes: Vec<usize>, total_cost: usize) -> Self {
        Self { nodes, total_cost }
    }
}


//
// fn determine_possible_moves_from(grid: &Grid<u32>, cell: usize, prev_cell: usize) -> Vec<CellRef> {
//     if prev_cell == pathfinding::astar::NO_NODE {
//         // Special case since below logic won't work when prev_cell == NO_NODE
//         // However we can assume only Right+Down are valid IFF we always start at top-left
//         return get_moves_in_dir(grid, cell, [GridDirection::Right, GridDirection::Down])
//     }
//
//     let current_travel_dir = grid.straight_direction_from(prev_cell, cell)
//         .unwrap_or_else(|| panic!("Last move {} to {} was not valid", prev_cell, cell));
//
//     match current_travel_dir {
//         GridDirection::Left | GridDirection::Right => get_moves_in_dir(grid, cell, [GridDirection::Up, GridDirection::Down]),
//         GridDirection::Up | GridDirection::Down => get_moves_in_dir(grid, cell, [GridDirection::Left, GridDirection::Right])
//     }
// }
//
// fn get_moves_in_dir(grid: &Grid<u32>, cell: usize, directions: [GridDirection; 2]) -> Vec<CellRef> {
//     let mut moves = grid.get_n_cells_in_direction(cell, directions[0], 3)
//         .iter().map(|cell| CellRef::new(*cell, directions[0])).collect_vec();
//
//     moves.extend(grid.get_n_cells_in_direction(cell, directions[1], 3).iter()
//                      .map(|cell| CellRef::new(*cell, directions[1])));
//     moves
// }
//
// fn path_cost(grid: &Grid<u32>, path: &Vec<usize>) -> usize {
//     path.windows(2)
//         .map(|el| cost_from(grid, el[0], el[1]))
//         .sum()
// }
//
// fn cost_from(grid: &Grid<u32>, from: usize, to: usize) -> usize {
//     let dir = grid.straight_direction_from(from, to)
//         .unwrap_or_else(|| panic!("Result path includes non-straight path segment from {} to {}", from, to));
//
//     let intermediate_cells = grid.get_cells_in_direction_until(from, dir, |next_cell, _| next_cell == to);
//
//     //println!("Cost {}->{} = ({} + {} + {})", grid.get(from), grid.get(to))
//     let a = (0 // (grid.get(from)
//         + intermediate_cells.iter().map(|ix| grid.get(*ix)).sum::<u32>()
//         + grid.get(to)) as usize;
// //    println!("Adding cost from {} to {} = {}", from, to, a);
//     a
// }

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
        let result = find_path(&parse_input("src/day17/test-input-1.txt"));
        assert_eq!((result.nodes, result.total_cost), (vec![0, 2, 8, 11], 7));
    }

    #[test]
    fn test_constrained_cost_pathfinding() {
        let result = find_path(&parse_input("src/day17/test-input-2.txt"));
        assert_eq!(result.total_cost, 102);
    }

    #[test]
    fn test_part1() {
        assert_eq!(part1(), 12);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(), 12);
    }

}
