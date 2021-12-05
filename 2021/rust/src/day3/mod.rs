use super::common;

pub fn run() {
    println!("Part 1 result: {}", part1());
}

fn part1() -> usize {
    let input = parse_input(common::read_file("src/day3/problem-input.txt"));
    let rates = calc_rates(&input);

    rates.epsilon as usize * rates.gamma as usize
}

#[allow(dead_code)]
fn dec_to_binary(mut n: u32) -> String {
    let mut s = String::new();
    while n != 0 {
        s.push(if n % 2 == 0 { '0' } else { '1' });
        n >>= 1;
    }

    s.chars().rev().collect()
}

fn binary_to_dec(bin: &str) -> u32 {
    bin.chars()
        .rev()
        .fold((0, 0), |(acc, pow), ch| (
            acc + if ch == '1' { 2u32.pow(pow) } else { 0 },
            pow + 1))
        .0
}

#[derive(Eq, PartialEq, Debug)]
struct Rates {
    pub gamma: u32,
    pub epsilon: u32
}

impl Rates {
    pub fn new(gamma: u32, epsilon: u32) -> Self {
        Self { gamma, epsilon }
    }
}

fn calc_rates(input: &Vec<String>) -> Rates {
    let threshold = input.len() as u32 / 2;
    let mut freq = [0u32; 12];

    input.iter().for_each(
        |b| b.chars().enumerate().for_each(|(i, c)| if c == '1' { freq[i] += 1 }));

    // Problem statement relies on all freq being above or below threshold
    assert!(freq.iter().all(|f| *f != threshold));

    let gamma_bin = freq.iter()
        .map(|f| if *f > threshold { '1' } else { '0' })
        .collect::<String>();

    let epsilon_bin = gamma_bin.chars()
        .map(|c| if c == '1' { '0' } else { '1' })
        .collect::<String>();

    Rates::new(binary_to_dec(&gamma_bin), binary_to_dec(&epsilon_bin))
}

fn parse_input(input: String) -> Vec<String> {
    input.lines().map(&str::to_string).collect()
}


#[cfg(test)]
mod test {
    use super::common;
    use crate::day3::{calc_rates, parse_input, part1, Rates};

    #[test]
    fn test_rate_calc() {
        let input = parse_input(common::read_file("src/day3/test-input.txt"));
        assert_eq!(calc_rates(&input), Rates::new(22, 4073));
    }

    #[test]
    fn test_part1() {
        assert_eq!(part1(), 12);
    }
}