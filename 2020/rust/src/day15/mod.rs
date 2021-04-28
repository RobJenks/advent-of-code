mod seq;

use super::common;
use crate::day15::seq::Seq;

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> usize {
    Seq::new(vec![2,1,10,11,0,6]).nth(2019).unwrap_or_else(|| panic!("No result"))
}

fn part2() -> usize {
    Seq::new(vec![2,1,10,11,0,6]).nth(29999999).unwrap_or_else(|| panic!("No result"))
}


#[cfg(test)]
mod tests {
    use crate::day15::seq::Seq;
    use crate::day15::part1;

    #[test]
    fn test_part1() {
        assert_eq!(232, part1());
    }

    #[test]
    fn test_seq_generation() {
        assert_eq!(vec![0,3,6,0,3,3,1,0,4,0], Seq::new(vec![0,3,6]).take(10).collect::<Vec<_>>())
    }

    #[test]
    fn test_seq_output() {
        assert_eq!(1, Seq::new(vec![1,3,2]).nth(2019).unwrap());
        assert_eq!(10, Seq::new(vec![2,1,3]).nth(2019).unwrap());
        assert_eq!(27, Seq::new(vec![1,2,3]).nth(2019).unwrap());
        assert_eq!(78, Seq::new(vec![2,3,1]).nth(2019).unwrap());
        assert_eq!(438, Seq::new(vec![3,2,1]).nth(2019).unwrap());
        assert_eq!(1836, Seq::new(vec![3,1,2]).nth(2019).unwrap());
    }
}