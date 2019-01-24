use std::collections::hash_set::HashSet;

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> u32 {
    redist_to_limit(parse_input("day6/input.txt"), 1)[0]
}

fn part2() -> u32 {
    redist_to_limit(parse_input("day6/input.txt"), 2)[1]
}


fn redist_to_limit(mut data: Vec<u32>, cycle_count: usize) -> Vec<u32> {
    let mut seen = HashSet::new();
    let mut results = Vec::<u32>::new();
    let n = data.len();

    while results.len() < cycle_count {
        let mut cycles = 0;

        while !seen.contains(&data) {
            seen.insert(data.clone());

            let src = data.iter().enumerate()
                .fold((0, 0), |(hi, hx), (i, x)| if x > &hx { (i, *x) } else { (hi, hx) });

            let mut ix = src.0;
            let mut val = src.1;

            data[ix] = 0;
            while val > 0 {
                ix = (ix + 1) % n;
                data[ix] += 1;
                val -= 1;
            }

            cycles += 1;
        }

        results.push(cycles);
        seen.clear();
    }

    results
}

fn parse_input(file: &str) -> Vec<u32> {
    common::read_file(file)
        .split_whitespace()
        .map(|x| x.parse::<u32>().unwrap())
        .collect::<Vec<u32>>()
}


#[cfg(test)]
mod tests {
    use super::{redist_to_limit};

    #[test]
    fn test_redist() {
        assert_eq!(redist_to_limit(vec![0, 2, 7, 0], 1), [5]);
    }

    #[test]
    fn test_loop_detection() {
        assert_eq!(redist_to_limit(vec![0, 2, 7, 0], 2), [5, 4]);
    }
}