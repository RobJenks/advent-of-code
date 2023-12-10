use std::collections::HashMap;

const ID_NONE : usize = usize::MAX;

#[derive(Clone, Copy)]
pub enum Direction {
    Left = 0,
    Right = 1
}

pub struct NodeId {
    pub index: usize,
    pub name: String,
}

pub struct Node {
    pub id: NodeId,
    pub connected: [NodeId; 2]
}

pub struct Data {
    pub directions: Vec<Direction>,
    pub nodes: Vec<Node>,
    pub indexed_nodes: HashMap<String, usize>
}



impl NodeId {
    pub fn new(name: String) -> Self {
        Self { name, index: ID_NONE }
    }
}

impl Node {
    pub fn new(id: NodeId, left: NodeId, right: NodeId) -> Self {
        Self { id, connected: [left, right] }
    }

    pub fn get_connected(&self, direction: Direction) -> &NodeId {
        &self.connected[direction as usize]
    }

    pub fn get_connected_index(&self, direction: Direction) -> usize {
        self.connected[direction as usize].index
    }
}

impl Data {
    pub fn new(directions: Vec<Direction>, nodes: Vec<Node>) -> Self {
        let mut data = Self { directions, nodes, indexed_nodes: HashMap::new() };

        data.nodes.iter_mut().enumerate().for_each(|(i, n)| {
            n.id.index = i;
            data.indexed_nodes.insert(n.id.name.clone(), i);
        });

        data.nodes.iter_mut().for_each(|n| {
           n.connected[Direction::Left as usize].index = *data.indexed_nodes.get(n.connected[Direction::Left as usize].name.as_str()).unwrap_or_else(|| panic!("Cannot resolve left pointer"));
           n.connected[Direction::Right as usize].index = *data.indexed_nodes.get(n.connected[Direction::Right as usize].name.as_str()).unwrap_or_else(|| panic!("Cannot resolve right pointer"));
        });

        data
    }
}