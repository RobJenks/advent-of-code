use std::cmp::Ordering;
use std::collections::HashSet;
use std::fmt::Display;
use std::hash::Hash;
use crate::common::graph::graph::{Graph, NO_NODE};
use crate::common::num::Numeric;
use crate::common::util::get_opposite_ordering;

pub struct PathFinder<'a, T, TPos, TCost>
    where T: Clone + Display,
          TPos: Clone + Display + Eq + PartialEq + Hash,
          TCost: Numeric + Clone + Display {

    graph: &'a Graph<T, TPos, TCost>
}

pub enum PathfindingType {
    BFS
}

pub enum PathEvalFn<T, TPos, TCost>
    where T: Clone + Display,
          TPos: Clone + Display + Eq + PartialEq + Hash,
          TCost: Numeric + Clone + Display {
    ShortestPath,
    LongestPath,
    LowestCost,
    HighestCost,

    // Should return Ordering::Greater for better result (first path data) than the current best (second path data)
    Custom(fn(&Graph<T, TPos, TCost>, &PathWorkingData<TCost>, &PathWorkingData<TCost>) -> Ordering)
}

pub struct PathResult<TCost> {
    pub start_node: usize,
    pub end_node: usize,
    pub found_path: bool,
    pub path: Vec<usize>,
    pub total_cost: TCost
}

impl<'a, T, TPos, TCost> PathFinder<'a, T, TPos, TCost>
    where T: Clone + Display,
          TPos: Clone + Display + Eq + PartialEq + Hash,
          TCost: Numeric + Clone + Display {

    pub fn new(graph: &'a Graph<T, TPos, TCost>) -> Self {
        Self { graph }
    }

    pub fn find_path(&self, start_node: usize, end_node: usize, path_type: PathfindingType,
                     path_eval_fn: PathEvalFn<T, TPos, TCost>) -> PathResult<TCost> {

        match path_type {
            PathfindingType::BFS => self.find_path_bfs(start_node, end_node, path_eval_fn)
        }
    }


    fn find_path_bfs(&self, start_node: usize, end_node: usize,
                         path_eval_fn: PathEvalFn<T, TPos, TCost>) -> PathResult<TCost> {

        let mut active = vec![ PathWorkingData::new_initial(start_node) ];
        let mut best_path : Option<PathWorkingData<TCost>> = None;

        while let Some(mut data) = active.pop() {
            println!("Processing node {} (current path: {:?}", self.graph.nodes[data.node], data.path);
            if data.node == end_node {
                data.found_path = true;
                if best_path.as_ref().map(|best| self.eval_path(&path_eval_fn, &data, best)).unwrap_or_else(|| Ordering::Greater) == Ordering::Greater {
                    println!("* Found new best path ({:?}), beats current best ({:?})", &data.path, best_path.as_ref().map(|b| b.path.clone()).unwrap_or_else(|| vec![]));
                    best_path = Some(data);
                }
                else {
                    println!("* Found path, but not better than current best.  Path = {:?}, Best = {:?}", &data.path, best_path.as_ref().unwrap().path)
                }
                continue;
            }

            if let Some(exits) = self.graph.get_connections_from(data.node) {
                if !exits.is_empty() {
                    for exit in &exits[0..(exits.len() - 1)] {
                        active.push(data.cloned_with(exit.target, exit.cost.clone()));
                    }

                    // Reuse and avoid cloning last
                    data.move_to(exits[exits.len() - 1].target, exits[exits.len() - 1].cost.clone());
                    active.push(data);
                }
            }
        }

        best_path.map(|best| PathResult::new(start_node, end_node, best.found_path, best.path, best.cost.clone()))
            .unwrap_or_else(|| PathResult::none())
    }

    fn eval_path(&self, path_eval_fn: &PathEvalFn<T, TPos, TCost>,
                 new_path: &PathWorkingData<TCost>, current_best_path: &PathWorkingData<TCost>) -> Ordering {

        if current_best_path.path == vec![0, 1, 2, 34, 27, 26, 25, 24, 19, 22, 32, 8, 9, 10, 11, 12, 13] &&
            new_path.path == vec![0, 1, 2, 34, 27, 26, 25, 24, 19, 18, 17, 16, 15, 14, 11, 12, 13] {

            println!("Divergence point");
        }

        match path_eval_fn {
            PathEvalFn::ShortestPath => current_best_path.path_length_if_complete().unwrap_or_else(|| usize::MAX)
                .cmp(&new_path.path_length_if_complete().unwrap_or_else(|| usize::MAX)),

            PathEvalFn::LongestPath => new_path.path_length_if_complete().unwrap_or_else(|| usize::MIN)
                .cmp(&current_best_path.path_length_if_complete().unwrap_or_else(|| usize::MIN)),

            eval_type @ (PathEvalFn::LowestCost | PathEvalFn::HighestCost) =>
                match &(new_path.cost_if_complete(), current_best_path.cost_if_complete(), eval_type) {
                    (Some(new), Some(best), PathEvalFn::LowestCost) => best.cmp(new),
                    (Some(new), Some(best), _ /* HighestCost */) => new.cmp(best),

                    (Some(_), None, _) => Ordering::Greater,
                    (None, Some(_), _) => Ordering::Less,
                    (None, None, _) => Ordering::Equal
                }

            PathEvalFn::HighestCost => get_opposite_ordering(
                self.eval_path(path_eval_fn, new_path, current_best_path)),

            PathEvalFn::Custom(f) => f(self.graph, new_path, current_best_path)
        }
    }
}

#[derive(Clone, Debug)]
struct PathWorkingData<TCost>
    where TCost: Numeric + Clone + Display {

    node: usize,
    cost: TCost,
    found_path: bool,
    path: Vec<usize>,
    visited: HashSet<usize>
}

impl<TCost> PathWorkingData<TCost>
    where TCost: Numeric + Clone + Display {

    pub fn new() -> Self {
        Self { node: NO_NODE, cost: TCost::zero(), found_path: false, path: Vec::new(), visited: HashSet::new() }
    }
    pub fn new_initial(node: usize) -> Self {
        let mut data = PathWorkingData::new();
        data.move_to(node, TCost::zero());
        data
    }
    pub fn new_with(node: usize, cost: TCost, found_path: bool, path: Vec<usize>, visited: HashSet<usize>) -> Self {
        Self { node, cost, found_path, path, visited }
    }

    pub fn cloned_with(&self, next_node: usize, cost: TCost) -> Self {
        let mut data = self.clone();
        data.move_to(next_node, cost);
        data
    }

    pub fn move_to(&mut self, node: usize, cost: TCost) {
        self.node = node;
        self.cost += cost;
        self.path.push(node);
        self.visited.insert(node);
    }

    pub fn mark_path_complete(&mut self) {
        self.found_path = true;
    }

    pub fn path_length_if_complete(&self) -> Option<usize> {
        if self.found_path { Some(self.path.len()) } else { None }
    }

    pub fn cost_if_complete(&self) -> Option<TCost> {
        if self.found_path { Some(self.cost.clone()) } else { None }
    }
}

impl<TCost> PathResult<TCost>
    where TCost: Numeric + Clone + Display {

    pub fn new(start_node: usize, end_node: usize, found_path: bool, path: Vec<usize>, total_cost: TCost) -> Self {
        Self { start_node, end_node, found_path, path, total_cost }
    }

    pub fn none() -> Self {
        Self::new(NO_NODE, NO_NODE, false, Vec::new(), TCost::zero())
    }
}

#[cfg(test)]
mod tests {
    use std::cmp::Ordering;
    use std::collections::HashSet;
    use itertools::Itertools;
    use crate::common::graph::graph::Graph;
    use crate::common::graph::pathfinder::{PathEvalFn, PathFinder, PathWorkingData};

    #[test]
    fn test_eval_fns() {
        let no_path = PathWorkingData::new_with(12, 34, false, vec![1,2,3,4,5], HashSet::new());
        let path_low_dist_high_cost = PathWorkingData::new_with(12, 500, true, vec![1,2], HashSet::new());
        let path_high_dist_low_cost = PathWorkingData::new_with(12, 5, true, (1..100).collect_vec(), HashSet::new());

        let graph = Graph::<usize, usize, isize>::new();
        let pathfinder = PathFinder::new(&graph);

        assert_eq!(pathfinder.eval_path(&PathEvalFn::ShortestPath, &path_low_dist_high_cost, &path_high_dist_low_cost), Ordering::Greater);
        assert_eq!(pathfinder.eval_path(&PathEvalFn::LongestPath, &path_low_dist_high_cost, &path_high_dist_low_cost), Ordering::Less);

        assert_eq!(pathfinder.eval_path(&PathEvalFn::ShortestPath, &path_high_dist_low_cost, &path_low_dist_high_cost), Ordering::Less);
        assert_eq!(pathfinder.eval_path(&PathEvalFn::LongestPath, &path_high_dist_low_cost, &path_low_dist_high_cost), Ordering::Greater);

        assert_eq!(pathfinder.eval_path(&PathEvalFn::ShortestPath, &no_path, &path_high_dist_low_cost), Ordering::Less);
        assert_eq!(pathfinder.eval_path(&PathEvalFn::ShortestPath, &path_high_dist_low_cost, &no_path), Ordering::Greater);

        assert_eq!(pathfinder.eval_path(&PathEvalFn::ShortestPath, &path_high_dist_low_cost, &path_high_dist_low_cost), Ordering::Equal);
        assert_eq!(pathfinder.eval_path(&PathEvalFn::ShortestPath, &path_low_dist_high_cost, &path_low_dist_high_cost), Ordering::Equal);
        assert_eq!(pathfinder.eval_path(&PathEvalFn::ShortestPath, &no_path, &no_path), Ordering::Equal);

        assert_eq!(pathfinder.eval_path(&PathEvalFn::LowestCost, &path_high_dist_low_cost, &path_low_dist_high_cost), Ordering::Greater);
        assert_eq!(pathfinder.eval_path(&PathEvalFn::LowestCost, &path_low_dist_high_cost, &path_high_dist_low_cost), Ordering::Less);
        assert_eq!(pathfinder.eval_path(&PathEvalFn::LowestCost, &path_low_dist_high_cost, &path_low_dist_high_cost), Ordering::Equal);
        assert_eq!(pathfinder.eval_path(&PathEvalFn::LowestCost, &path_low_dist_high_cost, &no_path), Ordering::Greater);
        assert_eq!(pathfinder.eval_path(&PathEvalFn::LowestCost, &no_path, &path_low_dist_high_cost), Ordering::Less);

        assert_eq!(pathfinder.eval_path(&PathEvalFn::HighestCost, &path_high_dist_low_cost, &path_low_dist_high_cost), Ordering::Less);
        assert_eq!(pathfinder.eval_path(&PathEvalFn::HighestCost, &path_low_dist_high_cost, &path_high_dist_low_cost), Ordering::Greater);
        assert_eq!(pathfinder.eval_path(&PathEvalFn::HighestCost, &path_low_dist_high_cost, &path_low_dist_high_cost), Ordering::Equal);
        assert_eq!(pathfinder.eval_path(&PathEvalFn::HighestCost, &path_low_dist_high_cost, &no_path), Ordering::Greater);
        assert_eq!(pathfinder.eval_path(&PathEvalFn::HighestCost, &no_path, &path_low_dist_high_cost), Ordering::Less);

        let custom = PathEvalFn::<usize, usize, isize>::Custom(|graph, new, best| {
            (best.cost - 35).abs().cmp(&(new.cost - 35))    // Cost criteria = proximity to 35
        });
        assert_eq!(pathfinder.eval_path(&custom, &path_low_dist_high_cost, &path_high_dist_low_cost), Ordering::Less);
        assert_eq!(pathfinder.eval_path(&custom, &path_high_dist_low_cost, &path_low_dist_high_cost), Ordering::Greater);
        assert_eq!(pathfinder.eval_path(&custom, &no_path, &path_low_dist_high_cost), Ordering::Greater);
        assert_eq!(pathfinder.eval_path(&custom, &no_path, &path_high_dist_low_cost), Ordering::Greater);
    }
}
