use super::common;
use itertools::Itertools;

const TARGET: u32 = 2020;

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> u64 {
    let sorted_input = sorted_input();

    sorted_input.iter().rev().map(|x| (x,        // Process each element from the tail of the list
        sorted_input.iter()            // Compare against head elements until the total would exceed the target
            .take_while(|&y| x + y <= TARGET)
            .find(|&y| x + y == TARGET)))

        .filter(|(_, y)| y.is_some())   // Return the product of the first pair meeting the criteria
        .map(|(&x, y)| (x * y.unwrap()) as u64)
        .next().unwrap_or_else(|| panic!("No result found"))
}

fn part2() -> u64 {
    let sorted_input = sorted_input();
    let eligible_pairs = eligible_pairs();

    sorted_input.iter().rev().map(|x| (x,       // Process each element from the tail of the list
        eligible_pairs.iter()       // Test against all eligible pairs which would remain under the target
            .filter(|(p0,p1)| x != p0 && x != p1)
            .find(|(p0,p1)| x+p0+p1 == TARGET)))

        .filter(|(_,p)| p.is_some())
        .map(|(x,p)| (x, p.unwrap().0, p.unwrap().1))
        .map(|(&x,y,z)| (x * y * z) as u64)
        .next().unwrap_or_else(|| panic!("No result found"))
}

// Return sorted input set
fn sorted_input() -> Vec<u32> {
    let mut input = common::read_file("src/day1/problem-input.txt")
        .lines()
        .map(|x| x.parse::<u32>().unwrap())
        .collect::<Vec<_>>();
    input.sort();

    input
}

// Return pairwise combinations that are below the target value, and therefore could
// form part of a triplet satisfying the criteria
fn eligible_pairs() -> Vec<(u32, u32)> {
    let input = sorted_input();
    input.iter()
        .permutations(2).unique()
        .map(|v| (v[0].to_owned(), v[1].to_owned()))
        .filter(|(x,y)| x+y < TARGET)
        .collect::<Vec<_>>()
}



#[cfg(test)]
mod tests {
    use super::{ part1, part2, sorted_input };

    #[test]
    fn test_part1() {
        assert_eq!(902451, part1());
    }

    #[test]
    fn test_part2() {
        assert_eq!(85555470, part2());
    }

    #[test]
    fn test_ensure_all_input_elements_are_unique() {
        assert_eq!(None, sorted_input().iter().zip(
                         sorted_input().iter().skip(1))
            .find(|(&x,&y)| x == y));
    }
}