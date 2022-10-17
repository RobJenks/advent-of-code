use crate::common;

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> usize {
    12
}

fn part2() -> usize {
    12
}

fn iterate(values: &mut Vec<u32>) {
    let n = values.len();
    for i in 0..n {
        let val = values[i];
        if val == 0 {
            values.push(8);
            values[i] = 6;
        }
        else {
            values[i] = val - 1;
        }
    }
}


fn parse_input(input: String) -> Vec<u32> {
    input.split(",")
        .map(|x| x.parse::<u32>())
        .map(|x| x.expect("Failed to parse input"))
        .collect()
}


#[cfg(test)]
mod test {
    use crate::day6::{part1, part2, iterate};

    #[test]
    fn test_iteration() {
        let mut values = vec![3,4,3,1,2];

        iterate(&mut values);
        assert_eq!(values, vec![2,3,2,0,1]);
        iterate(&mut values);
        assert_eq!(values, vec![1,2,1,6,0,8]);
        iterate(&mut values);
        assert_eq!(values, vec![0,1,0,5,6,7,8]);
        iterate(&mut values);
        assert_eq!(values, vec![6,0,6,4,5,6,7,8,8]);
    }

    #[test]
    fn test_part1() {
        assert_eq!(part1(), 12);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(), 12);
    }
}

