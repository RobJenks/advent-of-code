use std::collections::hash_set::HashSet;

pub fn run() {
    println!("Part 1 result: {}", part1());
}

fn part1() -> u32 {
    redist_to_limit(
        common::read_file("day6/input.txt")
            .split_whitespace()
            .map(|x| x.parse::<u32>().unwrap())
            .collect::<Vec<u32>>()
    )
}


fn redist_to_limit(mut data: Vec<u32>) -> u32 {
    let mut seen = HashSet::new();
    let mut cycles = 0;
    let n = data.len();

    while !seen.contains(&data) {
        seen.insert(data.clone());

        let src = data.iter().enumerate()
            .fold((0, 0), |(hi,hx), (i,x)| if x > &hx { (i, *x) } else { (hi, hx) } );

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

    cycles
}


#[cfg(test)]
mod tests {
    use super::{redist_to_limit};

    #[test]
    fn test_redist() {
        assert_eq!(redist_to_limit(vec![0, 2, 7, 0]), 5);
    }
}