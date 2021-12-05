use super::common;
use itertools::Itertools;
use crate::common::num::Zero;
use crate::common::vec2::Vec2;

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> i32 {
    evaluate_cmd_list(CommandMode::Standard,
        &parse_input(common::read_file("src/day2/problem-input.txt").as_str()))
}

fn part2() -> i32 {
    evaluate_cmd_list(CommandMode::Advanced,
        &parse_input(common::read_file("src/day2/problem-input.txt").as_str()))
}

fn evaluate_cmd_list(mode: CommandMode, commands: &Commands) -> i32 {
    commands.eval_iter(mode)
        .last()
        .map(|loc| loc.x * loc.y)
        .expect("Failed to evaluate command list")
}

#[derive(Debug, Eq, PartialEq)]
enum Command {
    Forward(i32),
    Up(i32),
    Down(i32),
}

struct Commands {
    pub data: Vec<Command>
}

impl Commands {
    pub fn new(data: Vec<Command>) -> Self { Self { data } }

    pub fn eval_iter(&self, mode: CommandMode) -> CommandsIterator {
        CommandsIterator {
            commands: self,
            index: 0,
            location: Vec2::<i32>::zero(),
            aim: 0,
            mode
        }
    }
}

struct CommandsIterator<'a> {
    commands: &'a Commands,
    index: usize,
    location: Vec2<i32>,
    aim: i32,
    mode: CommandMode
}

enum CommandMode {
    Standard,
    Advanced
}

impl <'a> IntoIterator for &'a Commands {
    type Item = Vec2<i32>;
    type IntoIter = CommandsIterator<'a>;

    fn into_iter(self) -> Self::IntoIter {
        CommandsIterator {
            commands: self,
            index: 0,
            location: Vec2::<i32>::zero(),
            aim: 0,
            mode: CommandMode::Standard
        }
    }
}

impl <'a> Iterator for CommandsIterator<'a> {
    type Item = Vec2<i32>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.index {
            x if x == self.commands.data.len() => None,
            _ => match self.mode {
                CommandMode::Standard => self.eval_standard(),
                CommandMode::Advanced => self.eval_advanced()
            }
        }
    }

}

impl <'a> CommandsIterator<'a> {
    fn eval_standard(&mut self) -> Option<Vec2<i32>> {
        let delta = match &self.commands.data[self.index] {
            Command::Forward(n) => Vec2::new(*n, 0),
            Command::Up(n) => Vec2::new(0, -*n),
            Command::Down(n) => Vec2::new(0, *n),
        };
        self.location += delta;
        self.index += 1;

        Some(self.location)
    }

    fn eval_advanced(&mut self) -> Option<Vec2<i32>> {
        match &self.commands.data[self.index] {
            Command::Up(n) => self.aim -= n,
            Command::Down(n) => self.aim += n,
            Command::Forward(n) => self.location += Vec2::new(*n, *n * self.aim)
        };
        self.index += 1;

        Some(self.location)
    }
}

fn parse_input(input: &str) -> Commands {
    Commands::new(input.lines()
        .map(parse_cmd_str)
        .collect())
}

fn parse_cmd_str(s: &str) -> Command {
    s.split_whitespace()
        .collect_tuple::<(&str, &str)>()
        .map(|(d, n)| parse_command(d, n.parse::<i32>().expect("Invalid non-numeric input")))
        .expect("Failed to parse command string")
}

fn parse_command(d: &str, n: i32) -> Command {
    match d {
        "forward" => Command::Forward(n),
        "up" => Command::Up(n),
        "down" => Command::Down(n),
        _ => panic!("Unrecognised command '{}'", d)
    }
}

#[cfg(test)]
mod test {
    use crate::day2::{CommandMode, evaluate_cmd_list, parse_input, part1, part2};

    fn sample_cmd_string() -> String {
        "forward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2".to_string()
    }

    #[test]
    fn test_cmd_eval() {
        assert_eq!(evaluate_cmd_list(CommandMode::Standard, &parse_input(sample_cmd_string().as_str())), 150);
    }

    #[test]
    fn test_adv_eval() {
        assert_eq!(evaluate_cmd_list(CommandMode::Advanced, &parse_input(sample_cmd_string().as_str())), 900);
    }

    #[test]
    fn test_part1() {
        assert_eq!(part1(), 1480518);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(), 1282809906);
    }
}