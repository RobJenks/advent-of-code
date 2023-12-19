use std::collections::HashMap;
use mut_binary_heap::{BinaryHeap, MinComparator};
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;


pub fn find_path<TNodeId: NodeId>
                (start: TNodeId, end: &[TNodeId],
                 get_connected: impl Fn(&TNodeId) -> Vec<TNodeId>,
                 get_cost: impl Fn(&TNodeId, &TNodeId) -> isize) -> Option<Vec<TNodeId>> {
    let mut open_list = BinaryHeap::<TNodeId, isize, MinComparator>::new();
    let mut nodes = HashMap::<TNodeId, Node<TNodeId>>::new();

    nodes.insert(start.clone(), Node::new(start.clone()));
    open_list.push(start.clone(), 0);

    while let Some((id, _)) = open_list.pop_with_key() {
        let node = nodes.get_mut(&id).unwrap_or_else(|| panic!("No record of node {}", id));
        let current_node_g = node.g;
        node.is_closed = true;

        if end.contains(&id) {
            break;  // Reached the target
        }

        for connected in get_connected(&id) {
            if !nodes.contains_key(&connected) {
                nodes.insert(connected.clone(), Node::new(connected.clone()));
            }
            let connected_node = nodes.get_mut(&connected).unwrap();
            if connected_node.is_closed { continue; }


            let new_child_g = current_node_g + get_cost(&id, &connected);
            let new_child_f = new_child_g + connected_node.h;

            // Add a new item to the open list if (A) this child is not yet on it, or (B) this route to the
            // child is better (in which case we are adding a replacement entry to the open list with lower
            // cost, so it will be produced first by the binary heap ahead of the worse route)
            if open_list.contains_key(&connected) {
                let mut current_cost = open_list.get_mut(&connected).unwrap();
                if new_child_g < connected_node.g {
                    *current_cost = new_child_f;
                    connected_node.f = new_child_f;
                    connected_node.g = new_child_g;
                    connected_node.parent = id.clone();
                }
            } else {
                open_list.push(connected.clone(), new_child_f);
                connected_node.f = new_child_f;
                connected_node.g = new_child_g;
                connected_node.h = 0; // Fall back to ~Dijkstra for now; calculating heuristic for d17 nodes would cost more than it saves
                connected_node.parent = id.clone();
            }
        }
    }

    // We have found a path if the end node has a parent pointer, and we can follow that all the way back
    if let Some(end_node) = end.iter()
        .filter_map(|end_id| nodes.get(end_id))
        .find(|&end_node| !end_node.parent.is_none()) {

        let mut path = Vec::new();
        let mut current_trace_node = end_node;
        loop {
            path.push(current_trace_node.id.clone());

            if current_trace_node.parent.is_none() { break }
            current_trace_node = &nodes.get(&current_trace_node.parent).unwrap_or_else(|| panic!("Missing parent node"));
        }

        path.reverse();
        Some(path)
    }
    else {
        None
    }
}

// NodeId
pub trait NodeId : PartialEq + Eq + Hash + Clone + Display {
    fn none() -> Self;
    fn is_none(&self) -> bool;
}

// Node

#[derive(Debug, Clone)]
struct Node<TNodeId: NodeId> {
    pub id: TNodeId,
    pub parent: TNodeId,
    pub f: isize,
    pub g: isize,
    pub h: isize,
    pub is_open: bool,
    pub is_closed: bool
}

impl<TNodeId: NodeId> Node<TNodeId> {
    pub fn new(id: TNodeId) -> Self {
        Self { id, parent: TNodeId::none(), f: 0, g: 0, h: 0, is_open: false, is_closed: false }
    }
}

impl<TNodeId: NodeId> Display for Node<TNodeId> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Node (id={}, parent={}, f={}, g={}, h={}{}{})",
            self.id,
            self.parent,
            self.f, self.g, self.h,
            if self.is_open { ", IsOpen" } else { "" },
            if self.is_closed { ", IsClosed" } else { "" })
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;
    use crate::common;
    use crate::common::grid::Grid;

    fn _load_test_grid(file: &str) -> Grid<u32> {
        Grid::new_from_2d_data(
            &common::read_file(file)
                .lines()
                .map(|s| s.trim().chars()
                    .map(|c| c.to_digit(10).unwrap_or_else(|| panic!("Invalid grid value")))
                    .collect_vec())
                .collect_vec())
    }

}
