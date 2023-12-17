use std::collections::BinaryHeap;
use std::fmt::Display;
use std::hash::Hash;
use crate::common::grid::Grid;

pub fn find_path<T: Eq + Hash + Copy + Display>(grid: &Grid<T>, start: usize, end: usize,
                                      neighbor_fn: fn(&Grid<T>, current_cell: usize) -> Vec<usize>,
                                      cost_fn: fn(&Grid<T>, current_cell: usize) -> isize) -> Vec<usize> {

    let mut open_list = BinaryHeap::<Node<T>>::new();
    open_list.push(Node::new(start, grid.get(start), 0));

    // https://www.redblobgames.com/pathfinding/a-star/implementation.html
    // https://medium.com/@nicholas.w.swift/easy-a-star-pathfinding-7e6689c7f7b2
    // https://github.com/sarkahn/sark_pathfinding_rs/blob/main/src/pathfinder.rs

    vec![]
}

#[derive(Eq, Debug)]
struct Node<T>
    where T: Eq + Hash + Copy + Display {
    pos: usize,
    value: T,
    cost: isize
}

impl<T: Eq + Hash + Copy + Display> Node<T> {
    pub fn new(pos: usize, value: T, cost: isize) -> Self {
        Self { pos, value, cost }
    }

}

impl<T: Eq + Hash + Copy + Display> PartialEq for Node<T> {
    fn eq(&self, other: &Self) -> bool {
        self.pos == other.pos && self.value == other.value && self.cost == other.cost
    }
}

impl<T: Eq + Hash + Copy + Display> PartialOrd for Node<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T: Eq + Hash + Copy + Display> Ord for Node<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        other.cost.cmp(&self.cost)
    }
}