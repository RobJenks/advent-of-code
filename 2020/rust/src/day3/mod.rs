use super::common;

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> u64 {
    evaluate(common::read_file("src/day3/problem-input.txt").as_str(), 3, 1)
}

fn part2() -> u64 {
    let input = common::read_file("src/day3/problem-input.txt");
    [(1usize,1usize), (3,1), (5,1), (7,1), (1,2)].iter()
        .map(|&(dx,dy)| evaluate(input.as_str(), dx, dy))
        .product()
}

fn evaluate(input: &str, dx: usize, dy: usize) -> u64 {
    // Pair every dy'th input line with a dx offset
    parse_input(input).iter().step_by(dy)
        .zip((0usize..).step_by(dx))

        // Evaluate the offset for each input line and sum the number of matches
        .map(|(v, ix)| v.iter().cycle().nth(ix).unwrap())
        .filter(|&&x| x)
        .count() as u64
}

fn parse_input(input: &str) -> Vec<Vec<bool>> {
    input.lines()
        .map(|x| x.chars().map(|c| c == '#').collect::<Vec<bool>>())
        .collect()
}


#[cfg(test)]
mod tests {
    use super::{ part1, part2 };

    #[test]
    fn test_part1() {
        assert_eq!(205, part1());
    }

    #[test]
    fn test_part2() {
        assert_eq!(3952146825, part2());
    }

}