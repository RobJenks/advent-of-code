use day18;

pub fn run() {
    println!("Part 1 result: {}", part1());
}

fn part1() -> usize {
    day18::CPU::new(8, day18::parse_instructions(common::read_file("day23/input.txt"), day18::APIVersion::V2)).by_ref()
        .filter(|x| match x.last {
            day18::Instruction::mul {x:_, y:_} => true,
            _ => false
        })
        .count()
}


#[cfg(test)]
mod tests {
    use super::{day18};

    #[test]
    fn test_regression() {
        assert_eq!(day18::part1(common::read_file("../day18/input.txt")), 9423);
        assert_eq!(day18::part2(common::read_file("../day18/input.txt")), 7620);
    }

}