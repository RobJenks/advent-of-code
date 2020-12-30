mod stream;
use super::common;
use crate::day9::stream::{Val, DataStream};

const PRELUDE_SIZE: usize = 25;

pub fn run() {
    println!("Part 1 result: {}", part1());
}

fn part1() -> u64 {
    DataStream::new(parse_input(common::read_file("src/day9/problem-input.txt")), PRELUDE_SIZE, PRELUDE_SIZE)
        .iter()
        .filter(|x| !x.is_valid())
        .map(|x| x.get_value())
        .next()
        .unwrap_or_else(|| panic!("No result found"))
}

fn parse_input(input: String) -> Vec<Val> {
    input.lines()
        .map(|x| x.parse::<Val>().unwrap_or_else(|e| panic!("Invalid input line '{}' ({})", x, e)))
        .collect()
}

#[cfg(test)]
mod tests {
    use crate::day9::stream::{DataStream, DataStreamIteratorResult};
    use crate::day9::part1;

    #[test]
    fn test_part1() {
        assert_eq!(756008079, part1())
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
}