use itertools::Itertools;
use std::collections::HashSet;
use super::common;

pub fn run() {
    println!("Part 1 result: {}", part1());
}

fn part1() -> usize {
    get_valid_passport_count(common::read_file("src/day4/problem-input.txt"))
}

fn get_valid_passport_count(input: String) -> usize {
    let mandatory_fields = mandatory_fields();

    parse_input(input).iter()
        .map(|x| Passport {fields: x.to_owned()})
        .filter(|x| x.has_fields(&mandatory_fields))
        .count()
}

fn parse_input(input: String) -> Vec<Vec<KeyValue>> {
    let params: Vec<String> = input.lines().collect::<Vec<_>>()
        .split(|&s| s.trim().is_empty())
        .map(|x| x.iter().map(|x| x.to_string()).join(" "))
        .collect();

    params.iter()
        .map(|s| s.split_whitespace()
            .map(|s| KeyValue::from(s.to_string()))
            .collect())
        .collect()
}

fn mandatory_fields() -> HashSet<String> {
    ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
        .iter().map(|x| x.to_string()).collect::<HashSet<_>>()
}

#[derive(Debug, Clone)]
struct KeyValue {
    key: String,
    value: String
}

#[derive(Debug)]
struct Passport {
    fields: Vec<KeyValue>
}

impl From<String> for KeyValue {
    fn from(x: String) -> Self {
        if let Some((k, v)) = x.split(":").collect_tuple() {
            Self { key: k.to_string(), value: v.to_string() }
        }
        else {
            panic!("Invalid key-value input string '{}'", x);
        }
    }
}

impl Passport {
    pub fn has_fields(&self, fields: &HashSet<String>) -> bool {
        self.fields.len() >= fields.len() &&

            self.fields.iter()
                .map(|kv| kv.key.clone())
                .collect::<HashSet<_>>().is_superset(fields)
    }
}

#[cfg(test)]
mod tests {
    use super::common;
    use super::{ part1, get_valid_passport_count };

    #[test]
    fn test_basic_validity() {
        assert_eq!(2, get_valid_passport_count(common::read_file("src/day4/test-input-1.txt")));
    }

    #[test]
    fn test_part1() {
        assert_eq!(202, part1());
    }

}