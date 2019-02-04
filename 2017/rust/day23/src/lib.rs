use day18;

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> usize {
    day18::CPU::new(8, day18::parse_instructions(common::read_file("day23/input.txt"), day18::APIVersion::V2)).by_ref()
        .filter(|x| match x.last {
            day18::Instruction::mul {x:_, y:_} => true,
            _ => false
        })
        .count()
}

fn part2() -> usize {
    // Reconstructed from CPU instructions, as required by part 2
    (0..=1_000)
        .map(|x| 108400 + (17 * x))
        .filter(|x| !is_prime(*x))
        .count()
}


fn is_prime(x: usize) -> bool {
    let mut div = 2usize;
    while div * div <= x {
        if x % div == 0 { return false };
        div += 1;
    }

    true
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