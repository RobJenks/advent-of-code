use itertools::Itertools;
use std::collections::HashSet;
use super::common;

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> usize {
    get_valid_passport_count(common::read_file("src/day4/problem-input.txt"),
                             &Passport::has_fields)
}

fn part2() -> usize {
    get_valid_passport_count(common::read_file("src/day4/problem-input.txt"),
                             &Passport::is_valid)
}

fn get_valid_passport_count(input: String, validation_fn: &dyn Fn(&Passport, &HashSet<String>) -> bool) -> usize {
    let mandatory_fields = mandatory_fields();

    parse_input(input).iter()
        .map(|x| Passport {fields: x.to_owned()})
        .filter(|x| validation_fn(x, &mandatory_fields))
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

impl KeyValue {
    pub fn get_key(&self) -> &String { &self.key }
    pub fn get_value(&self) -> &String { &self.value }
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

    pub fn is_valid(&self, mandatory_fields: &HashSet<String>) -> bool {
        self.has_fields(mandatory_fields) &&
            self.fields.iter().all(|x| Self::is_valid_field(x))
    }

    fn is_valid_field(field: &KeyValue) -> bool {
        match (field.get_key().as_str(), field.get_value().as_str()) {
            ("byr", x) => Self::is_valid_year(x, 1920, 2002),
            ("iyr", x) => Self::is_valid_year(x, 2010, 2020),
            ("eyr", x) => Self::is_valid_year(x, 2020, 2030),
            ("hgt", x) => Self::is_valid_height(x),
            ("hcl", x) => Self::is_valid_hcol(x),
            ("ecl", x) => Self::is_valid_ecol(x),
            ("pid", x) => Self::is_valid_pid(x),
            ("cid", _) => true,
            _ => false
        }
    }

    fn is_valid_year(s: &str, min: u32, max: u32) -> bool {
        s.len() == 4 &&
            s.parse::<u32>()
                .map(|v| v >= min && v <= max)
                .unwrap_or(false)
    }

    fn is_valid_height(s: &str) -> bool {
        s.len() > 2 &&
            s[0..s.len()-2].parse::<u32>()
                .map(|v| match &s[s.len()-2..s.len()] {
                    "cm" => v >= 150 && v <= 193,
                    "in" => v >= 59 && v <= 76,
                    _ => false
                })
                .unwrap_or(false)
    }

    fn is_valid_hcol(s: &str) -> bool {
        s.len() == 7 &&
            s.chars().skip(1).all(|c| (c >= 'a' && c <= 'f') || (c >= '0' && c <= '9'))
    }

    fn is_valid_ecol(s: &str) -> bool {
        ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"].contains(&s)
    }

    fn is_valid_pid(s: &str) -> bool {
        s.len() == 9 && s.parse::<u32>().is_ok()
    }
}

#[cfg(test)]
mod tests {
    use super::common;
    use super::{ part1, part2, Passport, get_valid_passport_count };

    #[test]
    fn test_basic_validity() {
        assert_eq!(2, get_valid_passport_count(
            common::read_file("src/day4/test-input-1.txt"),
            &Passport::has_fields));
    }

    #[test]
    fn test_part1() {
        assert_eq!(202, part1());
    }

    #[test]
    fn test_part2() {
        assert_eq!(137, part2());
    }

}