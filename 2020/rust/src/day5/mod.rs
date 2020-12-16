use super::common;
use itertools::Itertools;

const ROW_MIN: u32 = 0;
const ROW_MAX: u32 = 127;
const COL_MIN: u32 = 0;
const COL_MAX: u32 = 7;

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> u32 {
    common::read_file("src/day5/problem-input.txt").lines()
        .map(|s| evaluate(s))
        .max()
        .unwrap_or_else(|| panic!("Failed to evaluate any entries"))
}

fn part2() -> u32 {
    let ids = common::read_file("src/day5/problem-input.txt").lines()
        .map(|s| evaluate(s))
        .sorted()
        .collect::<Vec<_>>();

    ids.iter().zip(ids.iter().skip(1))
        .filter(|(&x,&y)| x+1 != y)
        .map(|(x,_)| x+1)
        .next()
        .unwrap_or_else(|| panic!("No result found"))
}

fn evaluate(s: &str) -> u32 {
    let (row, col) = s.chars()
        .fold((Range::new(ROW_MIN, ROW_MAX), Range::new(COL_MIN, COL_MAX)),
              |(row, col), x| match x {
                  'F' => (row.lower(), col),
                  'B' => (row.upper(), col),
                  'L' => (row, col.lower()),
                  'R' => (row, col.upper()),
                  _ => panic!("Unexpected value while parsing '{}'", x)
              });

    row.get_single() * 8 + col.get_single()
}

#[derive(Clone, Copy, Debug)]
struct Range {
    min: u32,
    max: u32,
}

impl Range {
    pub fn new(min: u32, max: u32) -> Self {
        Self { min, max }
    }

    fn size(&self) -> u32 {
        self.max - self.min + 1
    }

    pub fn lower(&self) -> Self {
        Self { min: self.min, max: self.min + (self.size() / 2 - 1) }
    }

    pub fn upper(&self) -> Self {
        Self { min: self.min + (self.size() / 2), max: self.max }
    }

    pub fn get_single(&self) -> u32 {
        if self.min == self.max { self.min }
        else { panic!("Range did not converge to a single value (min: {}, max: {})", self.min, self.max); }
    }
}


#[cfg(test)]
mod tests {
    use super::{ part1, part2, evaluate };

    #[test]
    fn test_basic_bsp() {
        assert_eq!(357, evaluate("FBFBBFFRLR"));
    }

    #[test]
    fn test_part1() {
        assert_eq!(816, part1());
    }
    #[test]
    fn test_part2() {
        assert_eq!(539, part2());
    }

}