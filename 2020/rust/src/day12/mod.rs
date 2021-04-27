mod commands;
mod navigator;

use super::common;
use commands::*;
use navigator::Navigator;
use crate::common::vec2::Vec2;
use crate::day12::navigator::NavigatorMode;

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> u32 {
    evaluate(NavigatorMode::DirectNavigation, parse_input(common::read_file("src/day12/problem-input.txt")))
        .manhattan_dist_from(&Vec2::new(0, 0))
}

fn part2() -> u32 {
    evaluate(NavigatorMode::WaypointNavigation, parse_input(common::read_file("src/day12/problem-input.txt")))
        .manhattan_dist_from(&Vec2::new(0, 0))
}

fn evaluate(mode: NavigatorMode, commands: Vec<Command>) -> Navigator {
    commands.iter()
        .fold(Navigator::new(mode), |x, c| x.execute_command(c))
}

fn parse_input(input: String) -> Vec<Command> {
    input.lines()
        .map(|s| Command::new(s))
        .collect()
}
