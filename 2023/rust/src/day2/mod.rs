mod game;

use std::iter::Iterator;
use super::common;
use game::*;

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> u32 {
    get_matching_games(
        &parse_input("src/day2/problem-input.txt"),
        |g| g.is_possible(&get_available_cubes()))
}

fn part2() -> u32 {
    get_power_sum(
        &parse_input("src/day2/problem-input.txt"))
}

fn get_matching_games(games: &Games, pred: fn(&Game) -> bool) -> u32 {
    games.iter(pred)
        .map(|g| g.get_id())
        .sum::<u32>()
}

fn get_available_cubes() -> Vec<Cubes> {
    vec![Cubes::new(Color::Red, 12), Cubes::new(Color::Green, 13), Cubes::new(Color::Blue, 14)]
}

fn get_power_sum(games: &Games) -> u32 {
    games.iter_all()
        .map(Game::get_power)
        .sum()
}

fn parse_input(file: &str) -> Games {
    Games::new(
        common::read_file(file)
            .lines()
            .map(parse_game)
            .collect())
}

fn parse_game(str : &str) -> Game {
    let (game_id, content) = str.split_once(":").unwrap_or_else(|| panic!("Invalid game format"));
    let id = game_id.split_once(" ").unwrap_or_else(|| panic!("No ID")).1
        .parse::<u32>().unwrap_or_else(|_| panic!("Non-numeric ID"));

    Game::new(id, content.split(";").map(parse_round).collect())
}

fn parse_round(str: &str) -> Round {
    Round::new(str.split(",")
        .map(|x| scan_fmt!(x, "{d} {s}", u32, String).unwrap_or_else(|_| panic!("Invalid round format")))
        .map(|(count, color)| Cubes::new(color.into(), count))
        .collect())
}

#[cfg(test)]
mod tests {
    use super::{part1, part2, parse_input, get_matching_games, get_available_cubes, get_power_sum};

    #[test]
    fn test_matching_games() {
        assert_eq!(get_matching_games(
            &parse_input("src/day2/test-input-1.txt"),
            |g| g.is_possible(&get_available_cubes())), 8);
    }

    #[test]
    fn test_power_sum() {
        assert_eq!(get_power_sum(
            &parse_input("src/day2/test-input-1.txt")), 2286);
    }

    #[test]
    fn test_part1() {
        assert_eq!(part1(), 2683);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(), 49710);
    }
}