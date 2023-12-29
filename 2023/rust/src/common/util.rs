#![allow(unused)]
use std::cmp::Ordering;

pub fn get_opposite_ordering(ordering: Ordering) -> Ordering {
    match ordering {
        Ordering::Greater => Ordering::Less,
        Ordering::Less => Ordering::Greater,
        Ordering::Equal => Ordering::Equal
    }
}