use super::common;

pub fn run() {
    println!("Part 1 result: {}", part1());
}

fn part1() -> u32 {
    let input = parse_input(common::read_file("src/day2/problem-input.txt"));
    input.iter()
        .filter(|x| x.is_valid())
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
    pub fn is_valid(&self) -> bool {
        let count = self.pwd.chars().filter(|&c| c == self.char).count() as u32;
        count >= self.min && count <= self.max
    }
}

#[cfg(test)]
mod tests {
    use super::{ part1 };

    #[test]
    fn test_part1() {
        assert_eq!(620, part1());
    }

}