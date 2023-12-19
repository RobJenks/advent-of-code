use std::collections::HashMap;
use mut_binary_heap::BinaryHeap;
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;
use itertools::Itertools;
use crate::common::grid::Grid;


pub fn find_path_spatial<T: Eq + Hash + Copy + Display>(grid: &Grid<T>, start: usize, end: usize,
                                      cost_fn: fn(&Grid<T>, from_cell: usize, to_cell: usize) -> isize) -> Option<Vec<usize>> {
    find_path(grid, BasicNodeKey::new(start), end,
        |grid, current_cell, _| grid.get_surrounding(current_cell).iter().cloned().map(BasicNodeKey::new).collect_vec(),
              cost_fn)
}

#[allow(unused)]
pub fn find_path_spatial_incl_diagonals<T: Eq + Hash + Copy + Display>(grid: &Grid<T>, start: usize, end: usize,
                                      cost_fn: fn(&Grid<T>, from_cell: usize, to_cell: usize) -> isize) -> Option<Vec<usize>> {
    find_path(grid, BasicNodeKey::new(start), end,
        |grid, current_cell, _| grid.get_surrounding_incl_diagonals(current_cell).iter().cloned().map(BasicNodeKey::new).collect_vec(),
              cost_fn)
}

pub fn find_path<T: Eq + Hash + Copy + Display, TNodeKey: NodeKey>
                (grid: &Grid<T>, start: TNodeKey, end: usize,
                 neighbor_fn: fn(&Grid<T>, current_cell: usize, prev_cell: usize) -> Vec<TNodeKey>,
                 cost_fn: fn(&Grid<T>, from_cell: usize, to_cell: usize) -> isize) -> Option<Vec<usize>> {

    let mut open_list = BinaryHeap::<TNodeKey, NodeRef<TNodeKey>>::new();
    // let mut nodes = grid.raw_data().iter()
    //     .map(|el| Node::new(*el))
    //     .collect_vec();

    let mut nodes = Nodes::<TNodeKey>::new();

    open_list.push(start.clone(), NodeRef::new(start.clone(), 0));
    while let Some(current_ref) = open_list.pop() {
        let current_node_g = nodes.get(&current_ref.key).g;
        let current_parent = nodes.get(&current_ref.key).parent.get_position();
        nodes.get_mut(&current_ref.key).unwrap().is_closed = true;
//println!("Open list = {}", open_list.iter().map(|x| format!("({}={})", x.pos, x.f_cost)).join(", "));
        if current_ref.key.get_position() == end {
            break;  // Reached the target
        }

        for child_ref in neighbor_fn(grid, current_ref.key.get_position(), current_parent) {
            nodes.get(&child_ref);
            let child = nodes.get_mut(&child_ref).unwrap();
            if child.is_closed { continue; }

            let new_child_g = current_node_g + cost_fn(grid, current_ref.key.get_position(), child_ref.get_position());
            let new_child_h = heuristic(grid, child_ref.get_position(), end);
            let new_child_f = new_child_g + new_child_h;

            // Add a new item to the open list if (A) this child is not yet on it, or (B) this route to the
            // child is better (in which case we are adding a replacement entry to the open list with lower
            // cost, so it will be produced first by the binary heap ahead of the worse route)
            //println!("Testing {} -> {}; new_g = {}, new_h = {}, new_f = {}, child = {}", current_ref.pos, child_ref.get_position(), new_child_g, new_child_h, new_child_f, child);
            if open_list.contains_key(&child_ref) {
                let mut existing = open_list.get_mut(&child_ref).unwrap();
                if new_child_g < child.g {
                    existing.f_cost = new_child_f;
                    child.f = new_child_f;
                    child.g = new_child_g;
                    child.h = new_child_h;
                    child.parent = current_ref.key.clone();
                    //println!("* New best route to {} is {}->{} with cost {}", child_ref.get_position(), current_ref.key.get_position(), child_ref.get_position(), new_child_g);
                }
                else {
                    //println!("* Ignoring route to {} from {}->{} with cost {} since better cost of {} for {}->{}",
                    //         child_ref.get_position(), current_ref.key.get_position(), child_ref.get_position(), new_child_g, child.g, child.parent.get_position(), child_ref.get_position());
                }
            }
            else {
                open_list.push(child_ref.clone(), NodeRef::new(child_ref.clone(), new_child_f));
                child.f = new_child_f;
                child.g = new_child_g;
                child.h = new_child_h;
                child.parent = current_ref.key.clone();
                //println!("* Route from {}->{} with cost {}", current_ref.key.get_position(), child_ref.get_position(), new_child_g);
            }

            // if !child.is_open || new_child_g < child.g {
            //     open_list.push(child_pos, NodeRef::new(child_pos, new_child_f));
            //     child.f = new_child_f;
            //     child.g = new_child_g;
            //     child.h = new_child_h;
            //     child.parent = current_ref.pos;
            //     //println!("* Adding (pos={}, f={}) to open list; now child = {}", child_pos, new_child_f, child);
            // }
        }
    }

    // We have found a path if the end node has a parent pointer, and we can follow that all the way back
    let mut node: Node<TNodeKey> = nodes.find_by_pos(end).cloned().unwrap_or_else(|| panic!("No end node"));
    if node.parent.get_position() == NO_NODE {
        None
    }
    else {
        let start_pos = start.get_position();
        let mut path = vec![end];
        loop {
            path.push(node.parent.get_position());
            if node.parent.get_position() == start_pos { break }
            node = nodes.get(&node.parent).clone();
        }
        path.reverse();
        Some(path)
    }
}

#[allow(unused)]
fn heuristic<T: Eq + Hash + Copy + Display>(grid: &Grid<T>, from_cell: usize, to_cell: usize) -> isize {
    // let from_pos = grid.ix_to_coord(from_cell);
    // let to_pos = grid.ix_to_coord(to_cell);
    // (to_pos.x as isize - from_pos.x as isize).abs() + (to_pos.y as isize - from_pos.y as isize).abs()
    0isize // grid.manhattan_dist(from_cell, to_cell) as isize
}

// Node

pub const NO_NODE : usize = usize::MAX;

#[derive(Debug, Clone)]
struct Node<TNodeKey>
    where //T: Eq + Hash + Copy + Display,
          TNodeKey: NodeKey {
    pub parent: TNodeKey,
    pub f: isize,
    pub g: isize,
    pub h: isize,
    pub is_open: bool,
    pub is_closed: bool
}

impl<TNodeKey: NodeKey> Node<TNodeKey> {
    pub fn new() -> Self {
        Self { parent: TNodeKey::none(), f: 0, g: 0, h: 0, is_open: false, is_closed: false }
    }
}

impl<TNodeKey: NodeKey> Display for Node<TNodeKey> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Node (parent={}, f={}, g={}, h={}{}{})",
            if self.parent.get_position() == NO_NODE { "None".to_string() } else { self.parent.get_position().to_string() },
            self.f, self.g, self.h,
            if self.is_open { ", IsOpen" } else { "" },
            if self.is_closed { ", IsClosed" } else { "" })
    }
}

// NodeRef

#[derive(Eq, Debug)]
struct NodeRef<TNodeKey: NodeKey> {
    key: TNodeKey,
    f_cost: isize
}

impl<TNodeKey: NodeKey> NodeRef<TNodeKey> {
    pub fn new(key: TNodeKey, f_cost: isize) -> Self {
        Self { key, f_cost }
    }
}

impl<TNodeKey: NodeKey> PartialEq for NodeRef<TNodeKey> {
    fn eq(&self, other: &Self) -> bool {
        self.key == other.key && self.f_cost == other.f_cost
    }
}

impl<TNodeKey: NodeKey> PartialOrd for NodeRef<TNodeKey> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<TNodeKey: NodeKey> Ord for NodeRef<TNodeKey> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        other.f_cost.cmp(&self.f_cost)
    }
}

// NodeKey
pub trait NodeKey : PartialEq + Eq + Hash + Clone + Debug {
    fn none() -> Self;
    fn get_position(&self) -> usize;
}

#[derive(Eq, Hash, Debug, Clone)]
pub struct BasicNodeKey {
    pub pos: usize
}

impl BasicNodeKey {
    pub fn new(pos: usize) -> Self {
        Self { pos }
    }
}

impl NodeKey for BasicNodeKey {
    fn none() -> Self {
        Self { pos: NO_NODE }
    }

    fn get_position(&self) -> usize {
        self.pos
    }
}

impl PartialEq for BasicNodeKey {
    fn eq(&self, other: &Self) -> bool {
        self.pos == other.pos
    }
}

// Nodes

struct Nodes<TNodeKey>
    where //T: Eq + Hash + Copy + Display,
          TNodeKey: NodeKey {
    pub data: HashMap<TNodeKey, Node<TNodeKey>>
}

impl<TNodeKey: NodeKey> Nodes<TNodeKey> {
    pub fn new() -> Self {
        Self { data: HashMap::new() }
    }

    pub fn get(&mut self, key: &TNodeKey) -> Node<TNodeKey> {
        if let Some(existing) = self.data.get(key) {
            existing.clone()
        }
        else {
            self.data.insert(key.clone(), Node::new());
            self.data.get(key).unwrap().clone()
        }
    }

    pub fn get_mut(&mut self, key: &TNodeKey) -> Option<&mut Node<TNodeKey>> {
        self.data.get_mut(key)
    }

    pub fn find_by_pos(&self, pos: usize) -> Option<&Node<TNodeKey>> {
        self.data.iter()
            .filter(|&(k, _)| k.get_position() == pos)
            .map(|(_, v)| v)
            .next()
    }
}


#[cfg(test)]
mod tests {
    use itertools::Itertools;
    use crate::common;
    use crate::common::grid::Grid;
    use crate::common::pathfinding::astar::find_path_spatial;

    fn load_test_grid(file: &str) -> Grid<u32> {
        Grid::new_from_2d_data(
            &common::read_file(file)
                .lines()
                .map(|s| s.trim().chars()
                    .map(|c| c.to_digit(10).unwrap_or_else(|| panic!("Invalid grid value")))
                    .collect_vec())
                .collect_vec())
    }

    #[test]
    fn test_basic_pathfinding() {
        let grid = load_test_grid("src/common/pathfinding/test-grid-1.txt");
        assert_eq!(find_path_spatial(&grid, 0, grid.get_element_count() - 1,
                          |g, from, to| g.manhattan_dist(from, to) as isize)

            .unwrap().len(), 8)
    }

    #[test]
    fn test_obstacle_pathfinding() {
        let grid = load_test_grid("src/common/pathfinding/test-grid-2.txt");
        assert_eq!(find_path_spatial(&grid, 0, grid.get_element_count() - 1,
                          |g, _, to| g.get(to) as isize),

            Some(vec![0, 6, 12, 13, 14, 8, 2, 3, 4, 10, 16, 17]));
    }


}
