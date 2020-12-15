use super::common;

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> u32 {
    parse_input(common::read_file("src/day2/problem-input.txt"))
        .iter()
        .filter(|x| x.is_valid_policy1())
        .count() as u32
}

fn part2() -> u32 {
    parse_input(common::read_file("src/day2/problem-input.txt"))
        .iter()
        .filter(|x| x.is_valid_policy2())
        .count() as u32
}

fn parse_input(input: String) -> Vec<Instance>{
    input.lines()
        .map(|s| scan_fmt!(s, "{d}-{d} {[a-z]}: {}", u32, u32, char, String)
            .unwrap_or_else(|e| panic!("Instance ({}) did not match expected input format: {}", s, e)))
        .map(|(min,max,char,pwd)| Instance{min, max ,char, pwd})
        .collect()
}

#[derive(Debug)]
struct Instance {
    min : u32,
    max : u32,
    char : char,
    pwd : String
}

impl Instance {
    pub fn is_valid_policy1(&self) -> bool {
        let count = self.pwd.chars().filter(|&c| c == self.char).count() as u32;
        count >= self.min && count <= self.max
    }

    pub fn is_valid_policy2(&self) -> bool {
        (self.at_index(self.min-1) == self.char) ^    // First char XOR
        (self.at_index(self.max-1) == self.char)      // second char matches
    }

    fn at_index(&self, i: u32) -> char {
        self.pwd.chars().nth(i as usize).unwrap_or_else(|| panic!("Index {} is invalid for {}", i, self.pwd))
    }
}

#[cfg(test)]
mod tests {
    use super::{ part1, part2 };

    #[test]
    fn test_part1() {
        assert_eq!(620, part1());
    }

    #[test]
    fn test_part2() {
        assert_eq!(727, part2());
    }

}