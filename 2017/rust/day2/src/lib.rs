pub fn run() {
    println!("Part 1 result: {}", part1());

}

fn part1() -> u32 {
    let input : Vec<String> = common::read_file("day2/input.txt").split("\n").map(|x| x.to_string()).collect();

    input.iter()
        .map(|x| x.split_whitespace()
            .map(|x| x.parse::<u32>().unwrap())
            .fold((std::u32::MAX, 0), |(low, high), x| (std::cmp::min(low, x), std::cmp::max(high, x)))
        )
        .map(|(low, high)| (high - low))
        .sum()
}



#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
