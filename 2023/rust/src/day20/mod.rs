mod model;

use std::collections::HashMap;
use std::iter::Iterator;
use itertools::Itertools;
use crate::day20::model::{build_logic, Module, ModuleType, System};
use super::common;

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> usize {
    parse_input("src/day20/problem-input.txt")
        .push_button_n_times(1000)
        .product()
}

fn part2() -> usize {
    12
}

fn parse_input(file: &str) -> System {
    let defined_module_details = common::read_file(file).lines()
        .map(parse_module)
        .collect_vec();

    let inferred_module_details = defined_module_details.iter()
        .flat_map(|details| details.outputs.iter()
            .filter(|&out| defined_module_details.iter().find(|&d| &d.id == out).is_none()))
        .map(|inferred| ModuleDetails::new(inferred.clone(), ModuleType::NoOp, Vec::new()))
        .collect_vec();

    let module_details = defined_module_details.iter().chain(inferred_module_details.iter()).collect_vec();

    let module_index = module_details.iter().enumerate()
        .map(|(ix, m)| (m.id.clone(), ix))
        .collect::<HashMap::<String, usize>>();

    let mut inputs = HashMap::<usize, Vec<usize>>::new();
    for sender in &module_details {
        let sender_ix = module_index.get(&sender.id).unwrap_or_else(|| panic!("Missing sender '{}'", sender.id));
        sender.outputs.iter().for_each(|receiver| {
            let receiver_ix = module_index.get(receiver).unwrap_or_else(|| panic!("Missing receiver '{}'", receiver));
            if !inputs.contains_key(receiver_ix) {
                inputs.insert(*receiver_ix, Vec::new());
            }

            inputs.get_mut(receiver_ix).unwrap().push(*sender_ix);
        });
    }

    System::new(
        module_details.iter().enumerate()
            .map(|(ix, details)| {
                let inputs = inputs.get(&ix).cloned().unwrap_or_else(|| Vec::new());
                let outputs = details.outputs.iter().map(|id| module_index.get(id).cloned().unwrap()).collect_vec();
                let logic = build_logic(details.module_type, &inputs, &outputs);

                Module::new(ix, details.id.clone(), inputs, outputs, logic)
            })
            .collect_vec()
    )
}

fn parse_module(str: &str) -> ModuleDetails {
    let (id_type, outputs) = str.split_once(" -> ").unwrap_or_else(|| panic!("Invalid input format ({})", str));
    let (id, module_type) = match id_type {
        "broadcaster" => ("broadcaster".to_string(), ModuleType::Broadcast),
        s if s.starts_with("%") => (s[1..].to_string(), ModuleType::FlipFlop),
        s if s.starts_with("&") => (s[1..].to_string(), ModuleType::Conjunction),
        _ => panic!("Unrecognized module id '{}'", id_type)
    };

    ModuleDetails::new(id, module_type, outputs.split(",").map(|s| s.trim().to_string()).collect_vec())
}


struct ModuleDetails {
    id: String,
    module_type: ModuleType,
    outputs: Vec<String>
}
impl ModuleDetails {
    pub fn new(id: String, module_type: ModuleType, outputs: Vec<String>) -> Self {
        Self { id, module_type, outputs }
    }
}


#[cfg(test)]
mod tests {
    use crate::day20::{parse_input, part1, part2};
    use crate::day20::model::ExecutionResult;

    #[test]
    fn test_basic_evaluation() {
        assert_eq!(parse_input("src/day20/test-input-1.txt").push_button(),
            ExecutionResult::new(8, 4));
    }

    #[test]
    fn test_repeated_evaluation() {
        assert_eq!(parse_input("src/day20/test-input-1.txt").push_button_n_times(1000),
            ExecutionResult::new(8000, 4000));
    }

    #[test]
    fn test_stateful_evaluation() {
        assert_eq!(parse_input("src/day20/test-input-2.txt")
                       .push_button_n_times(1000),
                   ExecutionResult::new(4250, 2750));

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
