use super::common;
const TARGET: u32 = 2020;

pub fn run() {
    println!("Part 1 result: {}", part1());
}

fn part1() -> u32 {
    let sorted_input = sorted_input();

    sorted_input.iter().rev().map(|x| (x,        // Process each element from the tail of the list
        sorted_input.iter()            // Compare against head elements until the total would exceed the target
            .take_while(|&y| x + y <= TARGET)
            .find(|&y| x + y == TARGET)))

        .filter(|(_, y)| y.is_some())   // Return the product of the first pair meeting the criteria
        .nth(0)
        .map(|(&x, y)| x * y.unwrap())
        .unwrap_or_else(|| panic!("No result found"))
}

fn sorted_input() -> Vec<u32> {
    let mut input = common::read_file("src/day1/input.txt")
        .lines()
        .map(|x| x.parse::<u32>().unwrap())
        .collect::<Vec<_>>();
    input.sort();

    input
}



#[cfg(test)]
mod tests {
    use super::part1;

    #[test]
    fn test_part1() {
        assert_eq!(902451, part1());
    }
}