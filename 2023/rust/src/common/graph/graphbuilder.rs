use std::collections::HashMap;
use std::fmt::Display;
use std::marker::PhantomData;
use itertools::Itertools;
use crate::common::graph::graph::*;
use crate::common::grid::Grid;
use crate::common::num::Numeric;
use crate::common::vec::Vec2;

pub struct GraphBuilder<T, TCost, G>
    where T: Clone + Display,
          TCost: Numeric + Clone + Display,
          G: Clone + Display {

    _t: PhantomData<T>,
    _t_cost: PhantomData<TCost>,
    _g: PhantomData<G>,
}

#[allow(unused)]
pub enum GridStartMode {
    FirstWalkableCell,
    AtSpecificIndex(usize),
    AtSpecificPosition(Vec2<usize>)
}

#[allow(unused)]
pub enum GenerateNodesAt<G> {
    IntersectionsOnly,
    Custom(fn(&G, usize, &Vec<usize>) -> bool)
}

impl<T, TCost, G> GraphBuilder<T, TCost, G>
    where T: Clone + Display,
          TCost: Numeric + Clone + Display,
          G: Clone + Display{

    pub fn build_from_grid(grid: &Grid<G>, start_cell: GridStartMode,
                              get_node_value: &impl Fn(&G) -> T,
                              is_walkable: &impl Fn(&G, usize) -> bool,
                              get_cost: &impl Fn(&Grid<G>, usize, usize, &Vec<usize>) -> TCost,
                              generate_node_at: &GenerateNodesAt<G>,
                              should_generate_edge: &impl Fn(&Grid<G>, usize, usize, &Vec<usize>) -> bool) -> Graph<T, usize, TCost> {

        let mut nodes = Vec::<NodeData<T, usize>>::new();
        let mut edges = Vec::<EdgeData<usize, TCost>>::new();

        let start = GraphBuilder::<T, TCost, G>::find_start(&grid, &start_cell, is_walkable);
        let mut covered = vec![false; grid.get_element_count()];
        let nodes_by_grid_ix = HashMap::<usize, usize>::new();

        // Vec<start_pos, option<index of last node that the following steps should connect to>, path up to this point>
        let mut active: Vec<(usize, Option<usize>, Vec<usize>)> = vec![(start, None, Vec::new())];

        while let Some((current, last_node, path)) = active.pop() {
            let mut prev_node_id = last_node;

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
                    edges.extend(GraphBuilder::<T, TCost, G>::generate_edges(grid, nodes[prev_node].pos, current, &path, should_generate_edge, &get_cost));
                }

                prev_node_id = Some(new_node_id);
            }

            // If we reach an already-covered cell containing a node we should still connect to it
            surrounding.iter()
                .filter(|&adj| covered[*adj])
                .flat_map(|adj| nodes_by_grid_ix.get(adj))
                .filter(|&adj_node| !path.contains(adj_node))
                .for_each(|target_node_id| {
                    let target_node = nodes.get(*target_node_id).unwrap_or_else(|| panic!("Target node {} does not exist", target_node_id));
                    let new_edges = GraphBuilder::<T, TCost, G>::generate_edges(grid, current, target_node.pos, &path, should_generate_edge, &get_cost);
                    edges.extend(new_edges);
                });

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

    fn generate_edges(grid: &Grid<G>, prev_ix: usize, next_ix: usize, path: &Vec<usize>,
                      should_generate_edge: &impl Fn(&Grid<G>, usize, usize, &Vec<usize>) -> bool,
                      get_cost: &impl Fn(&Grid<G>, usize, usize, &Vec<usize>) -> TCost) -> Vec<EdgeData<usize, TCost>> {

        let mut edges = Vec::new();
        let path_section = path[path.iter().position(|x| *x == prev_ix)
            .unwrap_or_else(|| panic!("Path section start index {} does not exist in current path {:?}", prev_ix, path))..]
            .iter().cloned().collect_vec();

        if should_generate_edge(grid, prev_ix, next_ix, path) {
            edges.push(EdgeData::<usize, TCost>::new(prev_ix, next_ix, get_cost(grid, prev_ix, next_ix, &path_section)));
        }

        if should_generate_edge(grid, next_ix, prev_ix, path) {
            edges.push(EdgeData::<usize, TCost>::new(next_ix, prev_ix, get_cost(&grid, next_ix, prev_ix, &path_section)));
        }

        edges
    }
}