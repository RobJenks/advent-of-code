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

fn generate_valid(rules: &Vec<Rule>) -> HashSet<String> {
    eval_rule(0, rules)
        .into_iter()
        .collect::<HashSet<_>>()
}

fn eval_rule(id: usize, rules: &Vec<Rule>) -> Vec<String> {
    vec!["eval", "results", "here"]
}

fn parse_input(input: String) -> (Vec<Rule>, Vec<String>) {
    let parts = input
        .split("---")
        .map(&str::trim)
        .filter(|s| !s.is_empty())
        .collect_tuple::<(&str, &str)>()
        .expect("Malformed input");

    (parse_rules(parts.0), parse_data(parts.1))
}

fn parse_rules(input: &str) -> Vec<Rule> {
    let rules = input.lines().map(parse_rule).collect::<Vec<_>>();
    let mut indexed = vec![Rule::Invalid; rules.len()];

    rules
        .iter()
        .for_each(|(id, rule)| indexed[*id] = rule.clone());
    indexed
}

fn parse_rule(s: &str) -> (usize, Rule) {
    let comp = s
        .split(":")
        .collect_tuple::<(&str, &str)>()
        .expect("Malformed rule");
    let id = comp.0.parse::<usize>().expect("Invalid ID");

    (
        id,
        parse_rule_content(comp.1.split_whitespace().map(&str::to_string).collect_vec()),
    )
}

fn parse_rule_content(comp: Vec<String>) -> Rule {
    match &comp[..] {
        [x] if x.contains('\"') => Rule::Constant(x.clone().replace('\"', "")),
        _ => Rule::Or(
            comp.split(|s| s == "|")
                .map(|el| {
                    el.iter()
                        .map(|s| s.parse::<usize>().expect("Non-numeric"))
                        .collect::<Vec<_>>()
                })
                .collect::<Vec<_>>(),
        ),
    }
}

fn parse_data(input: &str) -> Vec<String> {
    input.lines().map(&str::to_string).collect::<Vec<String>>()
}

#[derive(Clone, Debug)]
enum Rule {
    Invalid,
    Constant(String),
    Or(Vec<Vec<usize>>),
}

