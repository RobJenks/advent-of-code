use super::common;

pub fn run() {
    println!("Part 1 result: {}", part1());
}

fn part1() -> u32 {
    // Generate tuples of input lines and the required offset
    parse_input(common::read_file("src/day3/problem-input.txt"))
        .iter().zip((0usize..).step_by(3))

        // Evaluate the offset for each input line and sum the number of matches
        .map(|(v, ix)| v.iter().cycle().nth(ix).unwrap())
        .filter(|&&x| x)
        .count() as u32
}

fn parse_input(input: String) -> Vec<Vec<bool>> {
    input.lines()
        .map(|x| x.chars().map(|c| c == '#').collect::<Vec<bool>>())
        .collect()
}