use std::iter::Iterator;
use itertools::Itertools;
use crate::common::graph::graph::*;
use crate::common::graph::graphbuilder::*;
use crate::common::graph::pathfinder::*;
use crate::common::grid::{Grid, GridDirection};
use super::common;

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> usize {
    find_longest_path(&parse_input("src/day23/problem-input.txt")).1
}

fn part2() -> usize {
12
}

fn parse_input(file: &str) -> Data {
    let grid = Grid::new_from_2d_data(
        &common::read_file(file)
            .lines()
            .map(|line| line.chars().collect_vec())
            .collect_vec());

    let start_pos = grid.get_row(0).unwrap_or_else(|| panic!("No data")).iter()
        .position(|c| *c == '.')
        .map(|col| grid.coords_to_ix(col, 0))
        .unwrap_or_else(|| panic!("No start"));

    let end_pos = grid.get_row(grid.get_size().y() - 1).unwrap_or_else(|| panic!("No data")).iter()
        .position(|c| *c == '.')
        .map(|col| grid.coords_to_ix(col, grid.get_size().y() - 1))
        .unwrap_or_else(|| panic!("No end"));
println!("Path = from {} {} to {} {}", start_pos, grid.ix_to_coord(start_pos), end_pos, grid.ix_to_coord(end_pos));
    let graph = GraphBuilder::build_from_grid(&grid, GridStartMode::FirstWalkableCell,
        /* get_node_value */ &|c| *c,
        /* is_walkable */ &|c, _| *c != '#',
        /* get_cost */ &|grid, from, to, path| {
            println!("Cost of path from {} to {} (with steps {:?}) = {}", from, to, path, path.len());
            path.len()
        },// + if path.contains(&to) { 0 } else { 1 },
        /* generate_node_at */ &GenerateNodesAt::Custom(|val, ix, adj| adj.len() != 2 || is_slope(*val)),
        /* should_generate_edge */ &should_generate_edge);

    Data::new(grid, graph, start_pos, end_pos)
}

fn find_longest_path(data: &Data) -> (Vec<usize>, usize) {
    let start_node = data.graph.get_node_at(&data.start_pos).unwrap_or_else(|| panic!("No start node")).id;
    let end_node = data.graph.get_node_at(&data.end_pos).unwrap_or_else(|| panic!("No end node")).id;
println!("Path = node {} to {}", start_node, end_node);
    let pathfinder = PathFinder::new(&data.graph);
    let result = pathfinder.find_path(start_node, end_node, PathfindingType::BFS, PathEvalFn::HighestCost);

    if !result.found_path { panic!("No solution found"); }

    result.path.windows(2)
        .map(|n| (n, data.graph.edges[n[0]].iter().find(|tgt| tgt.target == n[1]).unwrap()))
        .map(|(n, edge)| (n, edge, data.grid.ix_to_coord(data.graph.get_node(n[0]).unwrap().pos), data.grid.ix_to_coord(data.graph.get_node(n[1]).unwrap().pos)))
        .for_each(|(n, edge, p0, p1)| println!("Edge from {} {}  to {} {} = cost {}", n[0], p0, n[1], p1, edge.cost));

    (result.path, result.total_cost)
}

fn is_slope(c: char) -> bool {
    c == '<' || c == '>' || c == '^' || c == 'v'
}

fn should_generate_edge(grid: &Grid<char>, from: usize, to: usize, path: &Vec<usize>) -> bool {
    let from_val = grid.get(from);
    let to_val = grid.get(to);
    if from_val == '.' && to_val == '.' { return true }

    assert!(path.len() >= 2);
    let is_fwd = path.contains(&from);
    assert!((is_fwd && !path.contains(&to)) || (!is_fwd && !path.contains(&from)));

    let (path_element_of_from_or_to_cell, cell_ix_after_from_cell, cell_ix_before_to_cell);
    if is_fwd {
        path_element_of_from_or_to_cell = path.iter().position(|x| *x == from).unwrap_or_else(|| panic!("Invalid intermediate path"));  // == path el of FROM
        cell_ix_after_from_cell = if path_element_of_from_or_to_cell == path.len() - 1 { to } else { path[path_element_of_from_or_to_cell + 1] };
        cell_ix_before_to_cell = path[path.len() - 1];
    }
    else {
        path_element_of_from_or_to_cell = path.iter().position(|x| *x == to).unwrap_or_else(|| panic!("Invalid intermediate path"));  // == path el of TO
        cell_ix_before_to_cell = if path_element_of_from_or_to_cell == path.len() - 1 { from } else { path[path_element_of_from_or_to_cell + 1] };
        cell_ix_after_from_cell = path[path.len() - 1];
    }

    let from_dir = GridDirection::from_unit_movement(&grid.pos_offset_between(from, cell_ix_after_from_cell))
        .unwrap_or_else(|| panic!("Could not determine edge start connection type from path ({}->{}; {:?})", from, to, path));
    let to_dir = GridDirection::from_unit_movement(&grid.pos_offset_between(cell_ix_before_to_cell, to))
        .unwrap_or_else(|| panic!("Could not determine edge end connection type from path ({}->{}; {:?})", from, to, path));

    let from_ok = if from_val == '.' { true } else { is_in_slope_direction(from_val, from_dir) };
    let to_ok = if to_val == '.' { true } else { is_in_slope_direction(to_val, to_dir) };

    if (from == 118 && to == 95) || (from == 95 && to == 118) { // node 2
        println!("From: {}, to: {}, fromval: {}, toval: {}, from_dir: {}, to_dir: {}, from_ok: {}, to_ok: {}, from_in_slope_dir: {}, to_in_slope_dir: {}, path: {:?}",
            from, to, from_val, to_val, from_dir, to_dir, from_ok, to_ok, from_val == '.' || is_in_slope_direction(from_val, from_dir),
                 to_val == '.' || is_in_slope_direction(to_val, to_dir), path);
    }

    from_ok && to_ok
}

fn get_slope_direction(slope: char) -> GridDirection {
    match slope {
        '<' => GridDirection::Left,
        '^' => GridDirection::Up,
        '>' => GridDirection::Right,
        'v' => GridDirection::Down,
        _ => panic!("Unsupported slope type '{}'", slope)
    }
}

fn is_in_slope_direction(slope: char, dir: GridDirection) -> bool {
    dir == get_slope_direction(slope)
}

fn grid_with_graph_to_string(data: &Data) -> String {
    data.grid.to_string_fmt(&|c, i| {
        if *c == '#' {
            c.to_string()
        }
        else if let Some(node) = data.graph.get_node_at(&i) {
            return match node.id {
                x if x < 26 => (char::from('A' as u8 + x as u8)).to_string(),
                _ => 'X'.to_string()
            }
        }
        else {
            c.to_string()
        }
    })
}


struct Data {
    grid: Grid<char>,
    graph: Graph<char, usize, usize>,
    start_pos: usize,
    end_pos: usize
}

impl Data {
    pub fn new(grid: Grid<char>, graph: Graph<char, usize, usize>, start_pos: usize, end_pos: usize) -> Self {
        Self { grid, graph, start_pos, end_pos }
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;
    use crate::day23::{find_longest_path, grid_with_graph_to_string, parse_input, part1, part2};

    #[test]
    fn test_graph_build() {
        let data = parse_input("src/day23/test-input-1.txt");

        data.graph.nodes.iter().for_each(|n| println!("Node {} at {} {} = {} (edges: {})",
             n.id, n.pos, data.grid.ix_to_coord(n.pos), n.value, data.graph.edges[n.id].iter().map(|e| e.target.to_string()).join(", ")));
        data.graph.edges.iter().enumerate().for_each(|(i, e)| e.iter()
            .for_each(|edge| println!("Edge {}->{} = cost {}", i, edge.target, edge.cost)));

        println!("\n\n{}\n\n{}", data.grid.to_string(), grid_with_graph_to_string(&data));
    }

    #[test]
    fn test_pathfinding() {
        assert_eq!(
            find_longest_path(&parse_input("src/day23/test-input-1.txt")),
            (vec![0, 1, 2, 34, 27, 26, 25, 24, 19, 18, 17, 16, 15, 14, 11, 12, 13], 94));
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
