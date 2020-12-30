mod stream;
use super::common;
use crate::day9::stream::{Val, DataStream};

const PRELUDE_SIZE: usize = 25;

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> u64 {
    DataStream::new(parse_input(common::read_file("src/day9/problem-input.txt")), PRELUDE_SIZE, PRELUDE_SIZE)
        .iter()
        .filter(|x| !x.is_valid())
        .map(|x| x.get_value())
        .next()
        .unwrap_or_else(|| panic!("No result found"))
}

fn part2() -> u64 {
    let stream = DataStream::new(parse_input(common::read_file("src/day9/problem-input.txt")), PRELUDE_SIZE, PRELUDE_SIZE);
    let range = stream.contiguous_range_with_sum(756008079);

    range.iter().min().unwrap() + range.iter().max().unwrap()
}

fn parse_input(input: String) -> Vec<Val> {
    input.lines()
        .map(|x| x.parse::<Val>().unwrap_or_else(|e| panic!("Invalid input line '{}' ({})", x, e)))
        .collect()
}

#[cfg(test)]
mod tests {
    use crate::day9::stream::{DataStream, DataStreamIteratorResult};
    use crate::day9::{part1, part2};

    #[test]
    fn test_part1() {
        assert_eq!(756008079, part1())
    }

    #[test]
    fn test_part2() {
        assert_eq!(93727241, part2())
    }

    #[test]
    fn test_basic_validation() {
        let values = vec![35,20,15,25,47,40,62,55,65,95,102,117,150,182,127,219,299,277,309,576];
        let stream = DataStream::new(values.clone(), 5, 5);

        let results = stream.iter().collect::<Vec<_>>();
        assert_eq!(vec![&DataStreamIteratorResult::Invalid(127)], results.iter().filter(|&x| !x.is_valid()).collect::<Vec<_>>());
        assert_eq!(values.iter().filter(|&x| *x != 127).cloned().collect::<Vec<_>>(),
                   results.iter().filter(|&x| x.is_valid()).map(|x| x.get_value()).collect::<Vec<_>>())
    }

    #[test]
    fn test_contiguous_range() {
        let values = vec![35,20,15,25,47,40,62,55,65,95,102,117,150,182,127,219,299,277,309,576];
        let stream = DataStream::new(values.clone(), 5, 5);

        let result = stream.contiguous_range_with_sum(127);
        assert_eq!(&[15, 25, 47, 40], result);
    }
}