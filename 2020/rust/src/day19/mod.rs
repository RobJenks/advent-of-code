use std::collections::HashSet;

use itertools::Itertools;

use crate::common;

pub fn run() {
    //    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> usize {
    let (rules, data) = parse_input(common::read_file("src/day19/problem-input.txt"));
    let valid = generate_valid(&rules, None);

    data.iter().filter(|&x| valid.contains(x)).count()
}

fn part2() -> usize {
    let (mut rules, data) = parse_input(common::read_file("src/day19/problem-input.txt"));

    let v42 = generate_valid(&rules, Some(42));
    let v31 = generate_valid(&rules, Some(31));

    // *** Generate regex and then use equivalent of "<42>{2,5}<31>{1,4}" to allow reasonable level
    // of repeats ***

    12

    //    let prefix = (2..5).map(|n| vec![42usize; n]);
    //    let suffix = (1..4).map(|n| vec![31usize; n]);

    //    let combinations = prefix
    //        .cartesian_product(suffix)
    //        .into_iter()
    //        .map(|(pr, sf)| [&pr[..], &sf[..]].concat())
    //        .collect::<Vec<Vec<usize>>>();

    //    rules[0] = Rule::Or(combinations);
    //    let valid = generate_valid(&rules);
    //    println!("Rules: {}", valid.len());
}

fn generate_valid(rules: &Vec<Rule>, root: Option<usize>) -> HashSet<String> {
    let mut cache = vec![None; rules.len()];
    eval_rule(root.unwrap_or_else(|| 0), rules, &mut cache)
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
        .expect("Malformea input");

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
