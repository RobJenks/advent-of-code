use std::iter::Iterator;
use itertools::Itertools;
use crate::common::graph::graph::*;
use crate::common::graph::graphbuilder::*;
use crate::common::grid::{Grid, GridDirection};
use super::common;

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> usize {
12
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

    let start_pos = grid.get_row(0).unwrap_or_else(|| panic!("No data"))
        .iter().find_position(|&c| *c == '.').unwrap_or_else(|| panic!("No start")).0;

    let end_pos = grid.get_row(grid.get_size().y() - 1).unwrap_or_else(|| panic!("No data"))
        .iter().find_position(|&c| *c == '.').unwrap_or_else(|| panic!("No end")).0;

    let graph = GraphBuilder::build_from_grid(&grid, GridStartMode::FirstWalkableCell,
        /* get_node_value */ &|c| *c,
        /* is_walkable */ &|c, _| *c != '#',
        /* get_cost */ &|grid, from, to, path| path.len(),
        /* generate_node_at */ &GenerateNodesAt::Custom(|val, ix, adj| adj.len() != 2 || is_slope(*val)),
        /* generate_node_type */ &get_node_connection_type);

    Data::new(grid, graph, start_pos, end_pos)
}

fn is_slope(c: char) -> bool {
    c == '<' || c == '>' || c == '^' || c == 'v'
}

fn get_node_connection_type(grid: &Grid<char>, from: usize, to: usize, path: &Vec<usize>) -> GenerateNodeType {
    let from_val = grid.get(from);
    let to_val = grid.get(to);
    if from_val == '.' && to_val == '.' { return GenerateNodeType::TwoWay }

    assert!(path.len() >= 2);
    let from_dir = GridDirection::from_unit_movement(&grid.pos_offset_between(path[0], path[1]))
        .unwrap_or_else(|| panic!("Could not determine edge start connection type from path ({:?})", path));
    let to_dir = GridDirection::from_unit_movement(&grid.pos_offset_between(path[path.len() - 1], path[path.len() - 2]))
        .unwrap_or_else(|| panic!("Could not determine edge end connection type from path ({:?})", path));

    let from_ok = if from_val == '.' { true } else { is_in_slope_direction(from_val, from_dir) };
    let to_ok = if to_val == '.' { true } else { is_in_slope_direction(to_val, to_dir) };

    match (from_ok, to_ok) {
        (true, true) => GenerateNodeType::TwoWay,
        (true, false) => GenerateNodeType::OneWay,
        (false, _) => GenerateNodeType::None
    }
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
    use crate::day23::{parse_input, part1, part2};

    #[test]
    fn test_graph_build() {
        let data = parse_input("src/day23/test-input-1.txt");

        data.graph.nodes.iter().for_each(|n| println!("Node {} at {} = {}", n.id, n.pos, n.value));
        data.graph.edges.iter().enumerate().for_each(|(i, e)| e.iter()
            .for_each(|edge| println!("Edge {}->{} = cost {}", i, edge.target, edge.cost)));

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
