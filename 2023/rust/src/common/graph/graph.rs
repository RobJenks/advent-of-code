#![allow(unused)]
use std::collections::HashMap;
use std::fmt::Display;
use std::hash::Hash;
use itertools::Itertools;
use crate::common::grid::Grid;
use crate::common::num::Numeric;

pub struct Graph<T, TPos, TCost>
    where T: Clone,
          TPos: Clone + Display + Eq + PartialEq + Hash,
          TCost: Numeric + Clone {

    pub nodes: Vec<Node<T, TPos>>,
    pub edges: Vec<Edges<TCost>>,    // Indexed by source node

    nodes_by_pos: HashMap<TPos, usize>
}

pub struct Node<T, TPos>
    where T: Clone,
          TPos: Clone + Eq + PartialEq + Hash {

    pub id: usize,
    pub value: T,
    pub pos: TPos

}

pub struct Edge<TCost>
    where TCost: Numeric + Clone {

    pub target: usize,
    pub cost: TCost
}

pub type Edges<TCost> = Vec<Edge<TCost>>;

// Caller-provided structs without derived internal data
#[derive(Debug, Clone)]
pub struct NodeData<T, TPos>
    where T: Clone,
          TPos: Clone + Eq + PartialEq + Hash {

    pub value: T,
    pub pos: TPos
}

#[derive(Debug, Clone)]
pub struct EdgeData<TPos, TCost>
    where TCost: Numeric + Clone,
          TPos: Clone + Eq + PartialEq + Hash {

    pub from: TPos,
    pub to: TPos,
    pub cost: TCost
}


// Impl

impl<T, TPos, TCost> Graph<T, TPos, TCost>
    where T: Clone,
          TPos: Clone + Display + Eq + PartialEq + Hash,
          TCost: Numeric + Clone {

    pub fn new() -> Self {
        Self::new_with(Vec::new(), Vec::new())
    }

    pub fn new_with(node_data: Vec<NodeData<T, TPos>>, edge_data: Vec<EdgeData<TPos, TCost>>) -> Self {
        let mut data = Self { nodes: Vec::new(), edges: Vec::new(), nodes_by_pos: HashMap::new() };

        node_data.iter().for_each(|d| _ = data.add_node_unchecked(d));
        edge_data.iter().for_each(|e| data.add_edge_between_pos_unchecked(&e.from, &e.to, e.cost.clone()));

        if let Err(e) = data.verify() {
            panic!("Invalid data provided for graph construction ({})", e);
        }

        data
    }

    pub fn add_node(&mut self, node_data: NodeData<T, TPos>) -> Result<usize, String> {
        if self.nodes_by_pos.contains_key(&node_data.pos) {
            return Err(format!("Cannot insert new node; a node already exists at position '{}'", node_data.pos.clone()));
        }

        Ok(self.add_node_unchecked(&node_data))
    }

    pub fn add_node_unchecked(&mut self, node_data: &NodeData<T, TPos>) -> usize {
        let new_id = self.nodes.len();
        self.nodes.push(Node::new(new_id, node_data.value.clone(), node_data.pos.clone()));
        self.nodes_by_pos.insert(node_data.pos.clone(), new_id);

        while self.edges.len() < self.nodes.len() {
            self.edges.push(Edges::new())
        }

        new_id
    }

    pub fn add_edge_between(&mut self, from: usize, to: usize, cost: TCost) -> Result<(), String> {
        if !self.is_valid_node_id(from) || !self.is_valid_node_id(to) {
            Err(format!("Cannot add edge between one or more invalid node IDs ({} -> {})", from, to))
        }
        else if let Some(_) = self.get_connections_from(from)
                                .map(|edges| edges.iter().find(|e| e.target == to)) {

            Err(format!("Cannot add edge from {} -> {}; edge already exists", from, to))
        }
        else {
            Ok(self.add_edge_unchecked(from, to, cost))
        }
    }

    pub fn add_edge_between_pos(&mut self, from: &TPos, to: &TPos, cost: TCost) -> Result<(), String> {
        let src = self.get_node_at(from);
        if src.is_none() { return Err(format!("Cannot add edge from source position '{}' which does not exist", from)) }

        let dest = self.get_node_at(to);
        if dest.is_none() { return Err(format!("Cannot add edge from dest position '{}' which does not exist", to)) }

        self.add_edge_between(src.unwrap().id, dest.unwrap().id, cost)
    }

    pub fn add_edge_unchecked(&mut self, from: usize, to: usize, cost: TCost) {
        self.edges[from].push(Edge::new(to, cost));
    }

    pub fn add_edge_between_pos_unchecked(&mut self, from: &TPos, to: &TPos, cost: TCost) {
        let src = self.nodes_by_pos[from];
        let dest = self.nodes_by_pos[to];

        self.add_edge_unchecked(src, dest, cost)
    }

    pub fn get_node_index(&self, pos: &TPos) -> Option<usize> {
        self.nodes_by_pos.get(&pos).cloned()
    }

    pub fn is_valid_node_id(&self, id: usize) -> bool {
        id < self.nodes.len()
    }

    pub fn get_node_at(&self, pos: &TPos) -> Option<&Node<T, TPos>> {
        self.nodes_by_pos.get(&pos)
            .map(|id| &self.nodes[*id])
    }

    pub fn get_node(&self, id: usize) -> Option<&Node<T, TPos>> {
        self.nodes.get(id)
    }

    pub fn get_node_mut(&mut self, id: usize) -> Option<&mut Node<T, TPos>> {
        self.nodes.get_mut(id)
    }

    pub fn get_node_mut_at(&mut self, pos: &TPos) -> Option<&mut Node<T, TPos>> {
        if let Some(id) = self.nodes_by_pos.get(&pos) {
            self.nodes.get_mut(*id)
        }
        else {
            None
        }
    }

    pub fn get_connections_from(&self, from: usize) -> Option<&Edges<TCost>> {
        self.edges.get(from)
    }

    pub fn get_connections_from_pos(&self, from: &TPos) -> Option<&Edges<TCost>> {
        if let Some(id) = self.get_node_index(from) {
            self.get_connections_from(id)
        }
        else {
            None
        }
    }

    pub fn verify(&self) -> Result<(), String> {
        if let Some((bad_ix, bad_node)) = self.nodes.iter().enumerate()
            .find(|&(i, n)| n.id != i) {
            Err::<(), String>(format!("Node at index {} has non-matching ID {}", bad_ix, bad_node.id));
        }

        else if let Some((bad_pos, bad_ix)) = self.nodes_by_pos.iter()
            .find(|&(pos, ix)| !self.is_valid_node_id(*ix) || &self.nodes[*ix].pos != pos) {
            Err::<(), String>(format!("Node pointer from position '{}' to node {} is invalid or node does not have matching position", bad_pos, bad_ix));
        }

        Ok(())
    }


}

impl<T, TPos> Node<T, TPos>
    where T: Clone,
          TPos: Clone + Eq + PartialEq + Hash {

    pub fn new(id: usize, value: T, pos: TPos) -> Self {
        Self { id, value, pos }
    }

}

impl<TCost> Edge<TCost>
    where TCost: Numeric + Clone {

    pub fn new(target: usize, cost: TCost) -> Self {
        Self { target, cost }
    }
}

impl<T, TPos> NodeData<T, TPos>
    where T: Clone,
          TPos: Clone + Eq + PartialEq + Hash {

    pub fn new(value: T, pos: TPos) -> Self {
        Self { value, pos }
    }
}

impl<TPos, TCost> EdgeData<TPos, TCost>
    where TCost: Numeric + Clone,
          TPos: Clone + Eq + PartialEq + Hash {

    pub fn new(from: TPos, to: TPos, cost: TCost) -> Self {
        Self { from, to, cost }
    }
}