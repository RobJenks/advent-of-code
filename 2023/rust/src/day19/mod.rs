mod model;

use std::iter::Iterator;
use itertools::Itertools;
use crate::day19::model::{Action, Comparison, Domain, Model, Part, Rule, Workflow};
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
    parse_input("src/day19/problem-input.txt")
        .determine_accepted_domain(Domain::new(1, 4001))
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
    use crate::day19::model::{Comparison, Domain, Field, Part};

    #[test]
    fn test_model_evaluation() {
        assert_eq!(parse_and_evaluate_model("src/day19/test-input-1.txt")
                       .get_accepted_parts().map(Part::sum_components).sum::<usize>(),
                   19114);
    }

    #[test]
    fn test_domain_split() {
        let (a1, b1) = Domain::new(1, 4001).split(&Comparison::LessThan(Field::A, 2000));
        assert_eq!(a1.to_tuples(), [(1, 4001), (1, 4001), (1, 2000), (1, 4001)]);
        assert_eq!(b1.to_tuples(), [(1, 4001), (1, 4001), (2000, 4001), (1, 4001)]);

        let (a1a, a1b) = a1.split(&Comparison::GreaterThan(Field::X, 500));
        assert_eq!(a1a.to_tuples(), [(501, 4001), (1, 4001), (1, 2000), (1, 4001)]);
        assert_eq!(a1b.to_tuples(), [(1, 501), (1, 4001), (1, 2000), (1, 4001)]);

        let (a1a_all, a1a_none) = a1a.split(&Comparison::None);
        assert_eq!(a1a_all.to_tuples(), a1a.to_tuples());
        assert_eq!(a1a_none.to_tuples(), [(0, 0), (0, 0), (0, 0), (0, 0)]);
    }

    #[test]
    fn test_out_of_range_domain_split() {
        let domain = Domain::new(1, 100);
        [Comparison::GreaterThan(Field::X, 100), Comparison::LessThan(Field::S, 0), Comparison::GreaterThan(Field::A, 1000000000)].iter()
            .map(|criteria| domain.split(criteria))
            .for_each(|(d0, d1)| {
                assert_eq!(d0.get_domain_size(), 0);
                assert_eq!(d1.to_tuples(), domain.to_tuples());
            });
    }

    #[test]
    fn test_domain_calculation() {
        assert_eq!(parse_input("src/day19/test-input-1.txt").determine_accepted_domain(
            Domain::new(1, 4001)), 167409079868000);
    }

    #[test]
    fn test_part1() {
        assert_eq!(part1(), 333263);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(), 130745440937650);
    }

}
