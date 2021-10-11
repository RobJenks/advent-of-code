use std::collections::HashSet;

use itertools::Itertools;

use crate::common;

pub fn run() {
    println!("Part 1 result: {}", part1());
}

fn part1() -> usize {
    let (rules, data) = parse_input(common::read_file("src/day19/problem-input.txt"));
    let valid = generate_valid(&rules);

    data.iter().filter(|&x| valid.contains(x)).count()
}

fn parse_input(input: String) -> (Vec<Rule>, Vec<String>) {
    (vec![Rule::Invalid], vec!["data"])
}


#[derive(Clone, Debug)]
enum Rule {
    Invalid,
    Constant(String),
    Or(Vec<Vec<usize>>),
}

