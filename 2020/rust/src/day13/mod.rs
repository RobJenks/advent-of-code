use super::common;

pub fn run() {
    println!("Part 1 result: {}", part1());
}

fn part1() -> u32 {
    get_best_departure(&parse_input(common::read_file("src/day13/problem-input.txt")))
        .map(|x| x.bus * x.delay)
        .unwrap_or_else(|| panic!("No result"))
}

fn get_best_departure(input: &Input) -> Option<Connection> {
    input.departures.iter()
        .filter_map(|&x| x)
        .map(|x| Connection { bus: x, delay: x - (input.depart_time % x) })
        .min_by(|x, y| x.delay.cmp(&y.delay))
}


#[derive(Debug)]
struct Input {
    pub depart_time: u32,
    pub departures: Vec<Option<u32>>
}

struct Connection {
    pub bus: u32,
    pub delay: u32
}

fn parse_input(input: String) -> Input {
    let depart_time = input.lines().nth(0)
        .map(|s| s.parse::<u32>().unwrap_or_else(|e| panic!("Failed to parse target: {}", e)))
        .unwrap_or_else(|| panic!("Cannot read departure time target"));

    let departures = input.lines().nth(1)
        .map(|s| s.split(',')
            .map(|x| x.parse::<u32>().map_or_else(|_| None, |n| Some(n)))
            .collect::<Vec<_>>())
        .unwrap_or_else(|| panic!("Cannot read departures"));

    Input { depart_time, departures }
}