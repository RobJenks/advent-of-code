use common::hash::{calculate_ascii_hash_binary};

const INPUT : &str = "hfdlxzhv";

pub fn run() {
    println!("Part 1 result: {}", part1(INPUT.to_string()));
}

fn part1(input: String) -> usize {
    (0..128)
        .map(|x|calculate_ascii_hash_binary(format!("{}-{}", input, x)))
        .map(|x| x.chars().filter(|c| *c == '1').count())
        .sum()
}



#[cfg(test)]
mod tests {
    use super::{calculate_ascii_hash_binary};

    #[test]
    fn test_hash() {
        let expected = vec!["11010100", "01010101", "00001010", "10101101", "01101000", "11001001", "01000100", "11010110"];
        let actual = (0..8).map(|x| calculate_ascii_hash_binary(format!("flqrgnkx-{}", x))).collect::<Vec<String>>();

        expected.iter().zip(actual.iter()).for_each(|(exp, act)| assert_eq!(*exp, &act[0..8]));
    }
}
