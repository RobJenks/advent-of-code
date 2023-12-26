use std::collections::HashMap;
use std::fmt::Display;
use std::hash::Hash;
use std::marker::PhantomData;
use itertools::Itertools;
use crate::common::graph::graph::*;
use crate::common::grid::Grid;
use crate::common::num::Numeric;
use crate::common::vec::Vec2;

pub struct GraphBuilder<T, TCost, G>
    where T: Clone + Display,
          TCost: Numeric + Clone,
          G: Clone + Display {

    _t: PhantomData<T>,
    _t_cost: PhantomData<TCost>,
    _g: PhantomData<G>,

}

pub enum GridStartMode {
    FirstWalkableCell,
    AtSpecificIndex(usize),
    AtSpecificPosition(Vec2<usize>)
}

pub enum GenerateNodesAt<G> {
    IntersectionsOnly,
    Custom(fn(&G, usize, &Vec<usize>) -> bool)
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum GenerateNodeType {
    None,
    OneWay,
    TwoWay
}

impl<T, TCost, G> GraphBuilder<T, TCost, G>
    where T: Clone + Display,
          TCost: Numeric + Clone,
          G: Clone + Display{

    pub fn build_from_grid(grid: &Grid<G>, start_cell: GridStartMode,
                              get_node_value: &impl Fn(&G) -> T,
                              is_walkable: &impl Fn(&G, usize) -> bool,
                              get_cost: &impl Fn(&Grid<G>, usize, usize, &Vec<usize>) -> TCost,
                              generate_node_at: &GenerateNodesAt<G>,
                              generate_node_type: &impl Fn(&Grid<G>, usize, usize, &Vec<usize>) -> GenerateNodeType) -> Graph<T, usize, TCost> {

        let mut nodes = Vec::<NodeData<T, usize>>::new();
        let mut edges = Vec::<EdgeData<usize, TCost>>::new();

        let start = GraphBuilder::<T, TCost, G>::find_start(&grid, &start_cell, is_walkable);
        let mut covered = vec![false; grid.get_element_count()];
        let mut nodes_by_grid_ix = HashMap::<usize, usize>::new();

        // Vec<start_pos, option<index of last node that the following steps should connect to>, path up to this point>
        let mut active: Vec<(usize, Option<usize>, Vec<usize>)> = vec![(start, None, Vec::new())];

        while let Some((from_ix, last_node, mut path)) = active.pop() {
            let mut current = from_ix;
            let mut prev_node_id = last_node;

            //loop {
            covered[current] = true;
            let val = grid.get(current);

            let surrounding = grid.get_surrounding(current).iter()
                .filter(|&c|is_walkable(&grid.get(*c), *c))
                .cloned().collect_vec();
            let exits = surrounding.iter().filter(|&i| !covered[*i]).cloned().collect_vec();

            if GraphBuilder::<T, TCost, G>::should_generate_node(&generate_node_at, &val, current, &surrounding) {
                if nodes_by_grid_ix.contains_key(&current) {
                    panic!("Cannot generate node at element {}; node already exists", current);
                }

                let new_node_id = nodes.len();
                nodes.push(NodeData::<T, usize>::new(get_node_value(&val), current));

                if let Some(prev_node) = prev_node_id {
                    let prev_node_grid_ix = nodes[prev_node].pos;
                    edges.extend(GraphBuilder::<T, TCost, G>::generate_edges(grid, prev_node_grid_ix, &grid.get(prev_node_grid_ix), prev_node,
                                                              current, &val, new_node_id, &path, &generate_node_type, &get_cost));
                }

                prev_node_id = Some(new_node_id);
            }

            // If we reach an already-covered cell containing a node we should still connect to it
            if let Some(prev_node) = prev_node_id {
                if !path.is_empty() && path[path.len() - 1] != nodes[prev_node].pos {   // Don't connect back to immediate last cell
                    surrounding.iter()
                        .filter(|&adj| covered[*adj])
                        .flat_map(|adj| nodes_by_grid_ix.get(adj))
                        .for_each(|target_node_id| {
                            let target_node = nodes.get(*target_node_id).unwrap_or_else(|| panic!("Target node {} does not exist", target_node_id));
                            let new_edges = GraphBuilder::<T, TCost, G>::generate_edges(grid, current, &val, prev_node, target_node.pos,
                                                                                        &grid.get(*target_node_id), *target_node_id, &path, &generate_node_type, &get_cost);
                            edges.extend(new_edges);
                        });
                }
            }

            let child_paths = exits.iter()
                .map(|ix| {
                    let mut new_path = path.clone();
                    new_path.push(current);
                    (*ix, prev_node_id, new_path)
                })
                .collect_vec();

            active.extend(child_paths);
        }

        Graph::<T, usize, TCost>::new_with(nodes, edges)
    }

    fn find_start(grid: &Grid<G>, start: &GridStartMode, is_walkable: impl Fn(&G, usize) -> bool) -> usize {
        match start {
            GridStartMode::AtSpecificIndex(ix) => *ix,
            GridStartMode::AtSpecificPosition(pos) => grid.coord_to_ix(pos),
            GridStartMode::FirstWalkableCell =>
                grid.raw_data().iter().enumerate()
                    .find(|&(i, c)| is_walkable(c, i))
                    .map(|(i, _)| i)
                    .unwrap_or_else(|| panic!("No start found")),
        }
    }

    fn should_generate_node(mode: &GenerateNodesAt<G>, val: &G, ix: usize, adjacent: &Vec<usize>) -> bool {
        match mode {
            GenerateNodesAt::IntersectionsOnly =>  adjacent.len() != 2,
            GenerateNodesAt::Custom(f) => f(val, ix, adjacent)
        }
    }

    fn generate_edges(grid: &Grid<G>,
                      prev_ix: usize, prev_value: &G, prev_node_id: usize,
                      next_ix: usize, next_value: &G, next_node_id: usize,
                      path: &Vec<usize>,
                      generate_node_type: &impl Fn(&Grid<G>, usize, usize, &Vec<usize>) -> GenerateNodeType,
                      get_cost: &impl Fn(&Grid<G>, usize, usize, &Vec<usize>) -> TCost) -> Vec<EdgeData<usize, TCost>> {

        let mut edges = Vec::new();

        let edge_type = generate_node_type(grid, prev_ix, next_ix, path);
        if edge_type == GenerateNodeType::OneWay || edge_type == GenerateNodeType::TwoWay {
            edges.push(EdgeData::<usize, TCost>::new(prev_ix, next_ix, get_cost(grid, prev_ix, next_ix, path)));
            if edge_type == GenerateNodeType::TwoWay {
                edges.push(EdgeData::<usize, TCost>::new(next_ix, prev_ix, get_cost(&grid, next_ix, prev_ix, path)));
            }
        }

        edges
    }
}