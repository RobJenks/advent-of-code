use super::common;

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> usize {
    evaluate_rates(calc_rates)
}

fn part2() -> usize {
    evaluate_rates(calc_collapsed_rates)
}

fn evaluate_rates(f: fn(&Vec<String>) -> Rates) -> usize {
    let input = parse_input(common::read_file("src/day3/problem-input.txt"));
    let rates = f(&input);

    rates.rate1 as usize * rates.rate0 as usize
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
    pub rate0: u32,
    pub rate1: u32
}

impl Rates {
    pub fn new(rate0: u32, rate1: u32) -> Self {
        Self { rate0, rate1 }
    }
}

fn calc_freq(input: &Vec<String>) -> Vec<u32> {
    let bin_width = input.iter().next().expect("No data").len();
    let mut freq = vec![0u32; bin_width];

    input.iter().for_each(
        |b| b.chars().enumerate().for_each(|(i, c)| if c == '1' { freq[i] += 1 }));

    freq
}

fn calc_rates(input: &Vec<String>) -> Rates {
    let freq = calc_freq(input);

    // Problem statement relies on all freq being above or below threshold
    let threshold = input.len() as u32 / 2;
    assert!(freq.iter().all(|f| *f != threshold));

    let gamma_bin = freq.iter()
        .map(|f| if *f > threshold { '1' } else { '0' })
        .collect::<String>();

    let epsilon_bin = gamma_bin.chars()
        .map(|c| if c == '1' { '0' } else { '1' })
        .collect::<String>();

    Rates::new(binary_to_dec(&gamma_bin), binary_to_dec(&epsilon_bin))
}

fn calc_collapsed_rates(input: &Vec<String>) -> Rates {
    let o2 = calc_collapsed_rate(input, |freq0, freq1| if freq1 >= freq0 { '1' } else { '0' });
    let co2 = calc_collapsed_rate(input, |freq0, freq1| if freq1 < freq0 { '1' } else { '0' });

    Rates::new(o2, co2)
}

fn calc_collapsed_rate(input: &Vec<String>, bin_criteria: fn(u32, u32) -> char) -> u32 {
    let mut eligible = input.clone();
    let bin_width = input.iter().next().expect("No data").len();
    let mut ix = 0usize;

    while eligible.len() != 1 {
        assert!(ix < bin_width);    // Exact solution exists within the input set, per problem statement

        let freq = calc_freq(&eligible);
        let criteria = bin_criteria(eligible.len() as u32 - freq[ix], freq[ix]);

        eligible = eligible.iter()
            .filter(|&b| b.chars().nth(ix).expect("Cannot index string") == criteria)
            .cloned()
            .collect();

        ix += 1;
    }

    assert_eq!(eligible.len(), 1);
    binary_to_dec(eligible.iter().next().expect("Invalid final result"))
 }

fn parse_input(input: String) -> Vec<String> {
    input.lines().map(&str::to_string).collect()
}


#[cfg(test)]
mod test {
    use super::common;
    use crate::day3::{calc_rates, calc_collapsed_rates, parse_input, part1, part2, Rates};

    #[test]
    fn test_rate_calc() {
        let input = parse_input(common::read_file("src/day3/test-input.txt"));
        assert_eq!(calc_rates(&input), Rates::new(22, 9));
    }

    #[test]
    fn test_collapsed_rate_calc() {
        let input = parse_input(common::read_file("src/day3/test-input.txt"));
        assert_eq!(calc_collapsed_rates(&input), Rates::new(23, 10));

    }

    #[test]
    fn test_part1() {
        assert_eq!(part1(), 3912944);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(), 4996233);
    }
}