use crate::common;

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> usize {
    12
}

fn part2() -> usize {
    12
}

#[cfg(test)]
mod test {
    use crate::day6::{part1, part2};

    #[test]
    fn test_part1() {
        assert_eq!(part1(), 12);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(), 12);
    }
}

