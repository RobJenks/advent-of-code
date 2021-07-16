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
    let mut cache = vec![None; rules.len()];
    eval_rule(0, rules, &mut cache)
        .into_iter()
        .collect::<HashSet<_>>()
}

fn eval_rule(id: usize, rules: &Vec<Rule>, cache: &mut Vec<Option<Vec<String>>>) -> Vec<String> {
    if let Some(cached) = cache[id].as_ref() {
        cached.clone()
    } else {
        let rule = &rules[id];
        let valid = match rule {
            Rule::Invalid => panic!("Invalid rule (id: {})", id),
            Rule::Constant(c) => vec![c.clone()],
            Rule::Or(groups) => groups
                .iter()
                .map(|grp| {
                    grp.iter()
                        .map(|r| eval_rule(*r, rules, cache))
                        .multi_cartesian_product()
                        .map(|vs| vs.join(""))
                        .collect::<Vec<_>>()
                })
                .map(|x| x)
                .flatten()
                .collect(),
        };

        assert!(cache[id].is_none());
        cache[id] = Some(valid.clone());

        valid
    }
}

// fn match_rule(s: &str, rules: &Vec<Rule>) -> Option<usize> {
//     rules
//         .iter()
//         .enumerate()
//         .filter(|(i, _)| matches_rule(s, *i, rules))
//         .map(|(i, _)| i)
//         .next()
// }
//
// fn matches_rule(s: &str, rule_id: usize, rules: &Vec<Rule>) -> bool {
//     let rule = &rules[rule_id];
//     match rule {
//         Rule::Invalid => panic!("Invalid rule (id: {})", rule_id),
//         Rule::Constant(c) => (s == c),
//         Rule::Or(groups) if groups.len() != 1 =>
//             groups.iter().any(|g| matches_rule_group(s, g, rules)),
//
//         Rule::Or(groups) if groups.len == 1 =>
//
//     }
// }
//
// fn matches_rule_group(s: &str, group: &Vec<usize>, rules: &Vec<Rule>) -> bool {
//     false
// }

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

#[cfg(test)]
mod tests {
    use crate::day18::*;
}
