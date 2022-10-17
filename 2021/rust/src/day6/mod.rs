use crate::common;
use std::collections::VecDeque;

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> usize {
    let mut values = parse_input(common::read_file("src/day6/problem-input.txt"));
    iterate_direct_n(&mut values, 80);

    values.len()
}

fn part2() -> usize {
    let values = parse_input(common::read_file("src/day6/problem-input.txt"));
    iterate_fast(&values, 256)
}

fn iterate_fast(values: &Vec<u32>, iterations: u32) -> usize {
    let mut days_remaining = [0usize; 9].into_iter().collect::<VecDeque<usize>>();

    values.iter().for_each(|&x| days_remaining[x as usize] += 1);

    (0..iterations).into_iter().for_each(|_| {
        days_remaining.rotate_left(1);
        days_remaining[6] += days_remaining[8];
    });

    days_remaining.iter().sum()
}

fn iterate_direct(values: &mut Vec<u32>) {
    let n = values.len();
    for i in 0..n {
        let val = values[i];
        if val == 0 {
            values.push(8);
            values[i] = 6;
        }
        else {
            values[i] = val - 1;
        }
    }
}

fn iterate_direct_n(values: &mut Vec<u32>, num_generations: u32) {
    (0..num_generations).into_iter().for_each(|_| iterate_direct(values));
}

fn parse_input(input: String) -> Vec<u32> {
    input.split(",")
        .map(|x| x.parse::<u32>())
        .map(|x| x.expect("Failed to parse input"))
        .collect()
}


#[cfg(test)]
mod test {
    use crate::day6::{part1, part2, iterate_direct, iterate_direct_n, iterate_fast};

    #[test]
    fn test_iteration() {
        let mut values = vec![3,4,3,1,2];

        iterate_direct(&mut values);
        assert_eq!(values, vec![2,3,2,0,1]);
        iterate_direct(&mut values);
        assert_eq!(values, vec![1,2,1,6,0,8]);
        iterate_direct(&mut values);
        assert_eq!(values, vec![0,1,0,5,6,7,8]);
        iterate_direct(&mut values);
        assert_eq!(values, vec![6,0,6,4,5,6,7,8,8]);
    }

    #[test]
    fn test_multi_generation() {
        let mut values = vec![3,4,3,1,2];

        iterate_direct_n(&mut values, 18);
        assert_eq!(values.len(), 26);
    }

    #[test]
    fn test_multi_generation_longer() {
        let mut values = vec![3,4,3,1,2];

        iterate_direct_n(&mut values, 80);
        assert_eq!(values.len(), 5934);
    }

    #[test]
    fn test_fast_iteration() {
        let values = vec![3,4,3,1,2];

        assert_eq!(iterate_fast(&values, 18), 26);
        assert_eq!(iterate_fast(&values, 80), 5934);
    }

    #[test]
    fn test_fast_iteration_exponential() {
        let values = vec![3,4,3,1,2];

        assert_eq!(iterate_fast(&values, 256), 26984457539);
    }

    #[test]
    fn test_part1() {
        assert_eq!(part1(), 377263);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(), 1695929023803);
    }
}

