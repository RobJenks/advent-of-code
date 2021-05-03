use crate::common;
use crate::day18::expr::Expr;

mod expr;

pub fn run() {
    println!("Part 1 result: {}", part1());
}

fn part1() -> isize {
    parse_input(common::read_file("src/day18/problem-input.txt")).iter()
        .map(|e| e.eval())
        .sum()
}

fn parse_input(input: String) -> Vec<Expr> {
    input.lines()
        .map(Expr::parse)
        .collect()
}

#[cfg(test)]
mod tests {
    use crate::day18::part1;

    #[test]
    fn test_part1() {
        assert_eq!(45283905029161, part1());
    }

}