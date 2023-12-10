mod nodes;

use std::iter::Iterator;
use itertools::Itertools;
use crate::day8::nodes::{Data, Direction, Node, NodeId};
use super::common;
use super::common::math::lcm;

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> usize {
    get_steps_to_end(&parse_input("src/day8/problem-input.txt"))
}

fn part2() -> usize {
    get_parallel_steps_to_end(&parse_input("src/day8/problem-input.txt"))
}

fn get_steps_to_end(data: &Data) -> usize {
    get_path_steps(data, &String::from("AAA"), |s| s == "ZZZ")
}

fn get_path_steps(data: &Data, start: &String, is_end: fn(&String) -> bool) -> usize {
    let mut index = *data.indexed_nodes.get(start).unwrap_or_else(|| panic!("Cannot find start"));

    let mut directions = data.directions.iter().cycle();
    let mut step = 0;

    loop {
        if is_end(&data.nodes[index].id.name) { break step }

        let dir = *directions.next().unwrap();
        index = data.nodes[index].connected[dir as usize].index;

        step += 1;
    }
}

fn get_parallel_steps_to_end(data: &Data) -> usize {
    let path_lengths = data.nodes.iter()
        .filter(|&n| n.id.name.ends_with('A'))
        .map(|n| get_path_steps(data, &n.id.name, |s| s.ends_with('Z')))
        .collect_vec();

    lcm(&path_lengths)
}

fn parse_input(file: &str) -> Data {
    let input = common::read_file(file);
    Data::new(
         input.lines().next().unwrap_or_else(|| panic!("No data")).chars()
             .map(|c| if c == 'L' { Direction::Left } else { Direction::Right })
             .collect_vec(),

        input.lines().skip(2)
            .map(|line| scan_fmt!(line, "{} = ({}, {})", String, String, String).unwrap_or_else(|e| panic!("Invalid input format '{}': {}", line, e)))
            .map(|(id, left, right)| Node::new(NodeId::new(id.trim().to_string()), NodeId::new(left.trim().to_string()), NodeId::new(right.trim().to_string())))
            .collect_vec()
    )
}

#[cfg(test)]
mod tests {
    use crate::day8::{get_parallel_steps_to_end, get_steps_to_end, parse_input, part1, part2};

    #[test]
    fn test_step_calculation_1() {
        assert_eq!(get_steps_to_end(&parse_input("src/day8/test-input-1.txt")), 2);
    }

    #[test]
    fn test_step_calculation_2() {
        assert_eq!(get_steps_to_end(&parse_input("src/day8/test-input-2.txt")), 6);
    }

    #[test]
    fn test_parallel_step_calculation() {
        assert_eq!(get_parallel_steps_to_end(&parse_input("src/day8/test-input-3.txt")), 6);
    }

    #[test]
    fn test_part1() {
        assert_eq!(part1(), 14429);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(), 10921547990923);
    }

}
