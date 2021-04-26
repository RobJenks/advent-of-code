mod commands;
mod navigator;

use super::common;
use commands::*;
use navigator::Navigator;
use crate::common::vec2::Vec2;

pub fn run() {
    println!("Part 1 result: {}", part1());
}

fn part1() -> u32 {
    evaluate(parse_input(common::read_file("src/day12/problem-input.txt")))
        .manhattan_dist_from(&Vec2::new(0, 0))
}

fn evaluate(commands: Vec<Command>) -> Navigator {
    commands.iter()
        .fold(Navigator::new(), |x, c| x.execute_command(c))
}

fn parse_input(input: String) -> Vec<Command> {
    input.lines()
        .map(|s| Command::new(s))
        .collect()
}
