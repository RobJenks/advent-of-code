use crate::common;
use crate::day18::expr::Expr;

mod expr;

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> isize {
    parse_input(common::read_file("src/day18/problem-input.txt"), Expr::parse).iter()
        .map(|e| e.eval())
        .sum()
}

fn part2() -> isize {
    parse_input(common::read_file("src/day18/problem-input.txt"), parse_with_precedence).iter()
        .map(|e| e.eval())
        .sum()
}

fn parse_input(input: String, parser: fn(&str) -> Expr) -> Vec<Expr> {
    input.lines()
        .map(parser)
        .collect()
}

fn parse_with_precedence(s: &str) -> Expr {
    let with_prec = Expr::insert_precedence(s);
    Expr::parse(with_prec.as_str())
}

#[cfg(test)]
mod tests {
    use crate::day18::{part1, part2};

    #[test]
    fn test_part1() {
        assert_eq!(45283905029161, part1());
    }

    #[test]
    fn test_part2() {
        assert_eq!(216975281211165, part2());
    }

}