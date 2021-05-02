mod criteria;
mod ticket;
use super::common;
use crate::day16::criteria::{Criteria, FieldCriteria, InclusiveRange};
use itertools::Itertools;
use crate::day16::ticket::Ticket;
use std::collections::HashSet;

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> u32 {
    find_invalid_values(parse_input_criteria(common::read_file("src/day16/problem-input-criteria.txt")),
                        parse_tickets(common::read_file("src/day16/problem-input-nearby.txt")))
        .iter()
        .sum()
}

fn part2() -> usize {
    find_departure_field_values(parse_input_criteria(common::read_file("src/day16/problem-input-criteria.txt")),
                                parse_tickets(common::read_file("src/day16/problem-input-nearby.txt")),
                                 Ticket::new(vec![83,53,73,139,127,131,97,113,61,101,107,67,79,137,89,109,103,59,149,71]))
        .iter()
        .map(|x| *x as usize)
        .product()
}

fn find_invalid_values(criteria: Criteria, tickets: Vec<Ticket>) -> Vec<u32> {
    tickets.iter()
        .map(|t| t.get_values().iter()
            .filter(|&x| !criteria.accepts_value(*x)))
        .flatten()
        .cloned()
        .collect()
}

fn find_departure_field_values(criteria: Criteria, tickets: Vec<Ticket>, ticket: Ticket) -> Vec<u32> {
    determine_field_names(&criteria, tickets).iter()
        .filter(|(name, _)| name.starts_with("departure"))
        .map(|(_, ix)| *ticket.get_values().get(*ix).unwrap_or_else(|| panic!("Invalid field index")))
        .collect()
}

fn determine_field_names(criteria: &Criteria, tickets: Vec<Ticket>) -> Vec<(String, usize)> {
    let valid = tickets.iter()
        .filter(|t| is_valid_ticket(criteria, t))
        .collect::<Vec<_>>();

    let mut mappings = criteria.get_fields().iter()
        .map(|f| (f.clone(), get_accepted_fields(f, &valid)))
        .collect::<_>();

    let mut solution = vec![];
    while let Some(sol) = find_single_solution_field(&mappings) {
        if let Err(e) = check_for_dead_end_solution_result(&mappings) { panic!("{}", e); }

        let field = mappings.get(sol).unwrap_or_else(|| panic!("Invalid partial solution result: {}", sol));

        assert_eq!(1, field.1.len());
        let ix = field.1.get(0).unwrap_or_else(|| panic!("Invalid partial solution index"));

        solution.push((field.0.get_name().to_string(), *ix));
        mappings = remove_confirmed_solution(&mappings, field.0.get_name(), *ix);
    }

    if !mappings.is_empty() {
        panic!("No unambiguous solution; solved criteria: {}\nRemaining potential mappings: {}",
            solution.iter().map(|(n, i)| format!("\n{} -> {}", n, i)).collect::<String>(),
            mappings.iter().map(|(f, v)|
                format!("\n{} -> {:?}", f.str(), v.iter().sorted().collect::<Vec<_>>())).collect::<String>());
    }

    solution
}

fn find_single_solution_field(mappings: &Vec<(FieldCriteria, Vec<usize>)>) -> Option<usize> {
    find_n_solution_field(mappings, 1)
}

fn check_for_dead_end_solution_result(mappings: &Vec<(FieldCriteria, Vec<usize>)>) -> Result<(), String> {
    find_n_solution_field(mappings, 0)
        .map(|n| Err(format!("Invalid dead-end solution for field '{}'", mappings.get(n)
            .unwrap_or_else(|| panic!("Invalid error index: {}", n))
            .0.get_name())))
        .unwrap_or_else(|| Ok(()))
}

fn find_n_solution_field(mappings: &Vec<(FieldCriteria, Vec<usize>)>, n: usize) -> Option<usize> {
    mappings.iter()
        .position(|m| m.1.len() == n)
}

fn remove_confirmed_solution(mappings: &Vec<(FieldCriteria, Vec<usize>)>,
                             field_name: &str, field_index: usize) -> Vec<(FieldCriteria, Vec<usize>)> {
    mappings.iter()
        .filter(|(f, _)| f.get_name() != field_name)
        .map(|(f, ix)| (f.clone(), ix.iter().filter(|&i| *i != field_index).cloned().collect()))
        .collect()
}

fn get_accepted_fields(field_criteria: &FieldCriteria, tickets: &Vec<&Ticket>) -> Vec<usize> {
    tickets.iter()
        .map(|t| get_accepted_ticket_fields(field_criteria, t))
        .fold1(|x, y| x.intersection(&y).cloned().collect())
        .map(|x| x.iter().cloned().collect())
        .unwrap_or_else(|| panic!("Failed to collapse field indices"))
}

fn get_accepted_ticket_fields(field_criteria: &FieldCriteria, ticket: &Ticket) -> HashSet<usize> {
    ticket.get_values().iter().enumerate()
        .filter(|&(_, x)| field_criteria.accepts_value(*x))
        .map(|(i, _)| i)
        .collect()
}

fn is_valid_ticket(criteria: &Criteria, ticket: &Ticket) -> bool {
    ticket.get_values().iter()
        .all(|v| criteria.accepts_value(*v))
}

fn parse_input_criteria(input: String) -> Criteria {
    Criteria::new(
        input.lines()
            .map(|x| x.split(':').collect_tuple::<(&str,&str)>())
            .map(|x| x.unwrap_or_else(|| panic!("Invalid criteria")))
            .map(|(name, ranges)| FieldCriteria::new(name, parse_ranges(ranges)))
            .collect()
    )
}

fn parse_ranges(s: &str) -> Vec<InclusiveRange> {
    s.split("or")
        .map(|x| x.trim().split('-')
            .map(|v| v.parse::<u32>().unwrap_or_else(|e| panic!("Invalid range value ({})", e)))
            .collect_tuple::<(u32, u32)>())
        .map(|x| x.map(|(a,b)| InclusiveRange::new(a, b)).unwrap_or_else(|| panic!("Failed to create range")))
        .collect()
}

fn parse_tickets(input: String) -> Vec<Ticket> {
    input.lines()
        .map(|s| s.split(',')
            .map(|x| x.parse::<u32>().unwrap_or_else(|e| panic!("Invalid ticket value ({})", e)))
            .collect())
        .map(|v| Ticket::new(v))
        .collect()
}


#[cfg(test)]
mod tests {
    use crate::common;
    use crate::day16::{determine_field_names, parse_input_criteria, parse_tickets, part1, part2};
    use itertools::Itertools;

    #[test]
    fn test_part1() {
        assert_eq!(19070, part1());
    }

    #[test]
    fn test_part2() {
        assert_eq!(161926544831, part2());
    }

    #[test]
    fn test_mapping_solver() {
        assert_eq!(vec![("class".to_string(), 1), ("row".to_string(), 0), ("seat".to_string(), 2)].iter().sorted().collect::<Vec<_>>(),
                   determine_field_names(&parse_input_criteria(common::read_file("src/day16/test-input-criteria-1.txt")),
                                         parse_tickets(common::read_file("src/day16/test-input-nearby-1.txt"))).iter().sorted().collect::<Vec<_>>());
    }
}