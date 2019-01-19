pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> u32 {
    let input = common::read_file("day1/input.txt");

    input.chars()
        .map(|x| x.to_digit(10).unwrap())
        .fold((input.chars().last().unwrap().to_digit(10).unwrap(), 0),
              |(prev, sum), x| (x, (sum + if x == prev { x } else { 0 }))).1
}

fn part2() -> u32 {
    let input = common::read_file("day1/input.txt");

    input.chars().enumerate()
        .map(|(i, x)| (x, input.chars().nth((i + (input.chars().count() / 2)) % input.chars().count())))
        .filter(|(x, prev)| x == &prev.unwrap())
        .map(|(x, _)| x.to_digit(10).unwrap())
        .sum()
}
