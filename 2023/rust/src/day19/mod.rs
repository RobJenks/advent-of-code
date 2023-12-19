mod model;

use std::iter::Iterator;
use itertools::Itertools;
use crate::day19::model::{Action, Comparison, Model, Part, Rule, Workflow};
use super::common;

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> usize {
    parse_and_evaluate_model("src/day19/problem-input.txt")
        .get_accepted_parts().map(Part::sum_components).sum()
}

fn part2() -> usize {
    12
}

fn parse_input(file: &str) -> Model {
    let content = common::read_file(file);
    let mut lines = content.lines().map(|line| line.trim());

    let workflows = lines.by_ref().take_while(|&line| !line.is_empty())
        .map(parse_workflow)
        .collect_vec();

    let parts = lines.by_ref()
        .map(parse_part)
        .collect_vec();

    Model::new(workflows, parts)
}

fn parse_workflow(str: &str) -> Workflow {
    let (name, content) = str.split_once("{").unwrap_or_else(|| panic!("Invalid workflow format"));
    let rules = content[0..content.len()-1].split(",").collect_vec();

    Workflow::new(name.to_string(), rules.into_iter().map(parse_rule).collect_vec())
}

fn parse_rule(str: &str) -> Rule {
    if let Some((criteria, action)) = str.split_once(":") {
        Rule::new(Comparison::inequality_from_str(criteria), Action::from_str(action))
    }
    else {
        Rule::new(Comparison::None, Action::from_str(str))
    }
}

fn parse_part(str: &str) -> Part {
    scan_fmt!(str, "{{x={d},m={d},a={d},s={d}}}", usize, usize, usize, usize)
        .map(|(x, m, a, s)| Part::new(x, m, a, s))
        .unwrap_or_else(|e| panic!("Invalid part format '{}' ({})", str, e))
}

fn parse_and_evaluate_model(file: &str) -> Model {
    let mut model = parse_input(file);
    model.evaluate();
    model
}


#[cfg(test)]
mod tests {
    use crate::day19::{parse_and_evaluate_model, parse_input, part1, part2};
    use crate::day19::model::Part;

    #[test]
    fn test_model_evaluation() {
        assert_eq!(parse_and_evaluate_model("src/day19/test-input-1.txt")
                       .get_accepted_parts().map(Part::sum_components).sum::<usize>(),
                   19114);
    }

    #[test]
    fn test_part1() {
        assert_eq!(part1(), 12);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(), 12);
    }

}
