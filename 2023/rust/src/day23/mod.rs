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
    find_longest_path(&parse_input("src/day23/problem-input.txt", false)).1
}

fn part2() -> usize {
    find_longest_path(&parse_input("src/day23/problem-input.txt", true)).1
}

fn parse_input(file: &str, can_traverse_slopes: bool) -> Data {
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

    let graph = GraphBuilder::build_from_grid(&grid, GridStartMode::FirstWalkableCell,
        /* get_node_value */ &|c| *c,
        /* is_walkable */ &|c, _| *c != '#',
        /* get_cost */ &|_, _, _, path| path.len(),
        /* generate_node_at */ &GenerateNodesAt::Custom(|val, _ix, adj| adj.len() != 2 || is_slope(*val)),
        /* should_generate_edge */ &|grid, from, to, path| should_generate_edge(grid, from, to, path, can_traverse_slopes));

    Data::new(grid, graph, start_pos, end_pos)
}

fn find_longest_path(data: &Data) -> (Vec<usize>, usize) {
    let start_node = data.graph.get_node_at(&data.start_pos).unwrap_or_else(|| panic!("No start node")).id;
    let end_node = data.graph.get_node_at(&data.end_pos).unwrap_or_else(|| panic!("No end node")).id;

    let pathfinder = PathFinder::new(&data.graph);
    let result = pathfinder.find_path(start_node, end_node, PathfindingType::BFS, PathEvalFn::HighestCost);

    if !result.found_path { panic!("No solution found"); }

    (result.path, result.total_cost)
}

fn is_slope(c: char) -> bool {
    c == '<' || c == '>' || c == '^' || c == 'v'
}

fn should_generate_edge(grid: &Grid<char>, from: usize, to: usize, path: &Vec<usize>, can_traverse_slopes: bool) -> bool {
    let from_val = grid.get(from);
    let to_val = grid.get(to);
    if (from_val == '.' && to_val == '.') || can_traverse_slopes { return true }

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

fn _grid_with_graph_to_string(data: &Data) -> String {
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

#[allow(unused)]
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
    use crate::day23::{find_longest_path, parse_input, part1, part2};

    #[test]
    fn test_pathfinding() {
        assert_eq!(
            find_longest_path(&parse_input("src/day23/test-input-1.txt", false)),
            (vec![0, 1, 2, 34, 27, 26, 25, 24, 19, 18, 17, 16, 15, 14, 11, 12, 13], 94));
    }

    #[test]
    fn test_unconstrained_pathfinding() {
        assert_eq!(
            find_longest_path(&parse_input("src/day23/test-input-1.txt", true)),
            (vec![0, 1, 2, 3, 4, 5, 6, 7, 8, 32, 22, 19, 24, 25, 26, 29, 31, 16, 15, 14, 11, 12, 13], 154));
    }

    #[test]
    fn test_part1() {
        assert_eq!(part1(), 2438);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(), 6658);
    }

}
