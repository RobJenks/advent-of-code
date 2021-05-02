use std::collections::HashSet;
use crate::common::nvec::Vec3;
use itertools::Itertools;
use std::fmt::Debug;

pub trait SpatialNode
    : std::cmp::PartialOrd
    + std::cmp::Eq
    + std::hash::Hash
    + core::clone::Clone
    + std::fmt::Debug {

    type Coord: Clone + Debug;
    fn new(pos: Self::Coord) -> Self;
    fn get_position(&self) -> &Self::Coord;
    fn get_neighbours(&self) -> Vec<Self::Coord>;
}

#[derive(Debug, Clone)]
pub struct Space<Node>
    where Node: SpatialNode {

    active: HashSet<Node>
}

impl<Node> Space<Node>
    where Node: SpatialNode {

    pub fn new<T>(active: T) -> Self
        where T: IntoIterator<Item=Node>,
              Node: SpatialNode {
        Self { active: active.into_iter().collect() }
    }

    pub fn get_active(&self) -> &HashSet<Node> { &self.active }

    fn is_active(&self, node: &Node) -> bool {
        self.active.contains(node)
    }

    fn is_pos_active(&self, pos: &Node::Coord) -> bool {
        self.is_active(&Node::new(pos.clone()))
    }

    pub fn execute(&self) -> Self {
        let stay_active = self.active.iter()
            .filter(|&n| self.node_should_be_active(n, true))
            .collect::<Vec<_>>();

        let neighbours =  self.active.iter()
            .map(|adj| adj.get_neighbours().into_iter().map(|pos| Node::new(pos)))
            .flatten()
            .collect::<HashSet<_>>();

        let become_active = neighbours.iter()
            .filter(|&n| !self.is_active(n))
            .filter(|&n| self.node_should_be_active(n, false))
            .collect::<Vec<_>>();

        Self::new(stay_active.into_iter().merge(become_active.into_iter())
            .map(|n| n.to_owned())
            .collect::<HashSet<_>>())
    }

    pub fn execute_n(&self, n: usize) -> Self {
        (0..n).fold(self.clone(), |s, _| s.execute())
    }

    // Process an individual node at T and determine whether it should be active at T+1
    fn node_should_be_active(&self, node: &Node, currently_active: bool) -> bool {
        match node.get_neighbours().iter().filter(|adj| self.is_pos_active(adj)).count() {
            2 | 3 if currently_active => true,
            3     if !currently_active => true,
            _ => false
        }
    }
}
