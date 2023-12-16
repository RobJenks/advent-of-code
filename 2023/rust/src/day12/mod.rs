use std::collections::HashMap;
use std::iter::Iterator;
use std::ops::Add;
use itertools::Itertools;
use super::common;

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> usize {
    count_all_valid_permutations(&parse_input("src/day12/problem-input.txt"))
}

fn part2() -> usize {
    12
}

fn count_all_valid_permutations(data: &Vec<Springs>) -> usize {
    let mut cache = HashMap::new();
    data.iter()
        .map(|s| count_valid_permutations(s, &mut cache))
        .sum()
}

fn count_valid_permutations(data: &Springs, cache: &mut HashMap<State, usize>) -> usize {
    count_string_permutations(data.data.clone(), &data.groups, cache)
}

fn count_string_permutations(str: String, groups: &[u32], cache: &mut HashMap<State, usize>) -> usize {
    let state = State::new(str.clone(), groups.to_vec());
    if let Some(cached) = cache.get(&state) {
        return *cached;
    }

    // If we reach the end of the string, we are in a valid permutation IFF there are no more groups to find
    if state.str.is_empty() {
        return if state.groups.is_empty() { 1 } else { 0 }
    }

    //let mut permutations = 0;
    let permutations : usize;
    let next = state.str.chars().next().unwrap();

    if next == '.' {
        let count = state.str.chars().take_while(|c| *c == '.').count();
        permutations = count_string_permutations((&state.str[count..]).to_string(), &state.groups, cache);  // Skip all '.' chars in one go
    }
    else if next == '?' {
        // Diverges; the ? could either be intact or damaged
        permutations = count_string_permutations('.'.to_string().add(&str[1..]), groups, cache) +
                       count_string_permutations('#'.to_string().add(&str[1..]), groups, cache);
    }
    else { // next == '#'
        if groups.is_empty() {
            permutations = 0;   // No more groups to find, but we just encountered a damaged item
        }
        else {
            let expected = groups[0] as usize;
            if expected <= str.len() &&
                str[0..expected].chars().all(|c| c == '#' || c == '?') {
                // All chars in the next group are potentially valid (either definitely or possibly damaged)
                let next_groups = &groups[1..];
                if expected == str.len() {
                    permutations = if next_groups.is_empty() { 1 } else { 0 };
                }
                else {
                    let char_after_group = str.chars().skip(expected).next().unwrap();
                    if char_after_group == '.' {
                        // Char after this group is operational, which means this is a valid damaged group.  Can also skip the '.'
                        permutations = count_string_permutations(str[(expected + 1)..].to_string(), next_groups, cache);
                    } else if char_after_group == '?' {
                        // Char after this group is unknown, but we now know it must be operational since it follows this damaged group
                        permutations = count_string_permutations('.'.to_string().add(&str[(expected + 1)..]), next_groups, cache);
                    } else { // next char is '#'
                        // Cannot have another damaged item after the current group of damaged items
                        permutations = 0;
                    }
                }
            }
            else {
                // The next set of chars cannot match the current group
                permutations = 0;
            }
        }
    }

    cache.insert(state, permutations);
    permutations
}

fn parse_input(file: &str) -> Vec<Springs> {
        common::read_file(file)
            .lines()
            .map(parse_springs)
            .collect_vec()
}

fn parse_springs(str: &str) -> Springs {
    str.split_once(' ')
        .map(|(s, g)| Springs::new(
            s.to_string(),
            g.split(',').map(|n| n.parse::<u32>().unwrap_or_else(|_| panic!("Invalid groups"))).collect_vec()
        ))
        .unwrap_or_else(|| panic!("Invalid input format"))
}

struct Springs {
    pub data: String,
    pub groups: Vec<u32>
}

impl Springs {
    pub fn new(data: String, groups: Vec<u32>) -> Self {
        Self { data, groups }
    }
}

#[derive(Eq, PartialEq, Hash, Clone, Debug)]
struct State {
    pub str: String,
    pub groups: Vec<u32>
}

impl State {
    pub fn new(str: String, groups: Vec<u32>) -> Self {
        Self { str, groups }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use crate::day12::{count_all_valid_permutations, count_valid_permutations, parse_input, parse_springs, part1, part2};

    #[test]
    fn test_count_permutations() {
        let mut cache = HashMap::new();
        assert_eq!(count_valid_permutations(&parse_springs("???.### 1,1,3"), &mut cache), 1);

        cache.clear();
        assert_eq!(count_valid_permutations(&parse_springs(".??..??...?##. 1,1,3"), &mut cache), 4);

        cache.clear();
        assert_eq!(count_valid_permutations(&parse_springs("?###???????? 3,2,1"), &mut cache), 10);
    }

    #[test]
    fn test_count_all_valid_permutations() {
        assert_eq!(count_all_valid_permutations(&parse_input("src/day12/test-input-1.txt")), 21);
    }

    #[test]
    fn test_part1() {
        assert_eq!(part1(), 7716);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(), 12);
    }

}
