mod nodes;

use std::any::Any;
use std::iter::Iterator;
use itertools::Itertools;
use crate::day8::nodes::{Data, Direction, Node, NodeId};
use super::common;

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> usize {
    get_steps_to_end(&parse_input("src/day8/problem-input.txt"))
}

fn part2() -> usize {
    12
}

fn get_steps_to_end(data: &Data) -> usize {
    let target = *data.indexed_nodes.get("ZZZ").unwrap_or_else(|| panic!("Cannot find end"));
    let mut index = *data.indexed_nodes.get("AAA").unwrap_or_else(|| panic!("Cannot find start"));

    let mut directions = data.directions.iter().cycle();
    let mut step = 0;

    loop {
        if data.nodes[index].id.index == target { break step }

        let dir = *directions.next().unwrap();
        index = data.nodes[index].connected[dir as usize].index;

        step += 1;
    }
}

fn parse_input(file: &str) -> Data {
    let input = common::read_file(file);
    Data::new(
         input.lines().next().unwrap_or_else(|| panic!("No data")).chars()
             .map(|c| if c == 'L' { Direction::Left } else { Direction::Right })
             .collect_vec(),

        input.lines().skip(2)
            .map(|line| scan_fmt!(line, "{} = ({}, {})", String, String, String).unwrap_or_else(|e| panic!("Invalid input format '{}': {}", line, e)))
            .map(|(id, left, right)| Node::new(NodeId::new(id), NodeId::new(left), NodeId::new(right)))
            .collect_vec()
    )
}


#[cfg(test)]
mod tests {
    use crate::day8::{get_steps_to_end, parse_input, part1, part2};

    #[test]
    fn test_step_calculation_1() {
        assert_eq!(get_steps_to_end(&parse_input("src/day8/test-input-1.txt")), 2);
    }

    #[test]
    fn test_step_calculation_2() {
        assert_eq!(get_steps_to_end(&parse_input("src/day8/test-input-2.txt")), 6);
    }

    #[test]
    fn test_part1() {
        assert_eq!(part1(), 14429);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(), 12);
    }

}
