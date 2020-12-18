use super::common;
use std::collections::HashMap;
use itertools::Itertools;

pub fn run() {
    println!("Part 1 result: {}", part1());
}

fn part1() -> u32 {
    let (mut items, index_map) = parse_input(common::read_file("src/day7/problem-input.txt").as_str());
    get_all_indirect_parents("shiny gold", &mut items, &index_map).iter()
        .unique()
        .count() as u32
}

fn get_all_indirect_parents(name: &str, items: &Vec<Item>, index_map: &IndexMap) -> Vec<usize> {
    follow_parents(*index_map.get(name).unwrap_or_else(|| panic!("Cannot find item to derive indirect parents ({})", name)), items)
}

fn follow_parents(item: usize, items: &Vec<Item>) -> Vec<usize> {
    items.get(item).unwrap_or_else(|| panic!("Cannot follow parents to index {}", item))
        .get_contained_by().iter()
        .map(|parent| (*parent.get_target(), follow_parents(*parent.get_target(), items)))
        .fold(vec![], |acc,x| {
            [acc.as_slice(), vec![x.0].as_slice(), x.1.as_slice()].concat()
        })
}

fn parse_input(s: &str) -> (Vec<Item>, IndexMap) {
    let mut items = vec![];
    let mut index_map = IndexMap::new();

    s.lines()
        .map(|line| process_line(line))
        .for_each(|(name, contains)| {
            // Ensure primary item and all dependencies exist in the graph first
            ensure_exists(name.trim(), &mut items, &mut index_map);
            contains.iter().for_each(|cont| ensure_exists(cont.get_target().trim(), &mut items, &mut index_map));

            // Process the relation (name, contains_k) for all k
            contains.iter().for_each(|rel| {
                add_relation(name.trim(), rel.get_target().trim(), rel.get_count(), &mut items, &index_map);
            })
        });

    return (items, index_map);
}

fn ensure_exists(name: &str, items: &mut Vec<Item>, index_map: &mut IndexMap) {
    if !index_map.contains_key(name) {
        items.push(Item::new(name));
        index_map.insert(name.to_string(), items.len() - 1);
    }
}

fn process_line(s: &str) -> (String, Vec<RelationT<String>>) {
    if let Some((src, target)) = s.split(" bags contain ").collect_tuple::<(&str,&str)>() {
        if target.contains("no other bags") {
            (src.to_string(), vec![])       // No children
        }
        else {
            (src.to_string(), parse_children(target))
        }
    }
    else {
        panic!("Input line does not match expected pattern ({})", s);
    }
}

fn parse_children(s: &str) -> Vec<RelationT<String>> {
    s.trim().replace(".", "").split(", ")
        .map(|x| x.splitn(2, " ").collect_tuple::<(&str,&str)>().unwrap_or_else(|| panic!("Child parse fail ({})", s)))
        .map(|(n, x)| (n.parse::<u32>().unwrap_or_else(|e| panic!("Not numeric: {} ({})", n, e)), x.replace("bags", "").replace("bag", "")))
        .map(|(n, x)| RelationT::<String>::new(x, n))
        .collect::<Vec<_>>()
}

fn add_relation(parent: &str, child: &str, count: u32, items: &mut Vec<Item>, index_map: &IndexMap) {
    let parent_ix = *index_map.get(parent).unwrap_or_else(|| panic!("Cannot find parent '{}'", parent));
    let child_ix = *index_map.get(child).unwrap_or_else(|| panic!("Cannot find child '{}'", child));

    items.get_mut(parent_ix).unwrap_or_else(|| panic!("Cannot retrieve parent '{}' ({})", parent, parent_ix))
        .add_contains_relation(child_ix, count);

    items.get_mut(child_ix).unwrap_or_else(|| panic!("Cannot retrieve child '{}' ({})", child, child_ix))
        .add_contained_by_relation(parent_ix, count);
}

type IndexMap = HashMap<String, usize>;

#[derive(Debug)]
struct RelationT<T> {
    target: T,
    count: u32
}

type Relation = RelationT<usize>;

#[derive(Debug)]
struct Item {
    name: String,
    contains: Vec<Relation>,
    contained_by: Vec<Relation>
}

impl Item {
    pub fn new(name: &str) -> Self {
        Self { name: name.to_string(), contains: vec![], contained_by: vec![] }
    }

    pub fn _get_contains(&self) -> &Vec<Relation> { &self.contains }
    pub fn get_contained_by(&self) -> &Vec<Relation> { &self.contained_by }

    pub fn add_contains_relation(&mut self, contained: usize, count: u32) {
        self.contains.push(Relation::new(contained, count));
    }

    pub fn add_contained_by_relation(&mut self, contains: usize, count: u32) {
        self.contained_by.push(Relation::new(contains, count));
    }
}

impl <T> RelationT<T> {
    pub fn new(target: T, count: u32) -> Self {
        Self { target, count }
    }

    pub fn get_target(&self) -> &T { &self.target }
    pub fn get_count(&self) -> u32 { self.count }
}