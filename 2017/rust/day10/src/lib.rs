use common::hash::{Hash, calculate_ascii_hash};

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> u32 {
    let hash = Hash::new(256, common::read_file("day10/input.txt")
        .split(",").map(|x| x.parse::<usize>().unwrap()).collect::<Vec<usize>>(), false);

    hash.last().unwrap().iter()
        .take(2)
        .fold(1, |acc,x| acc * *x )
}


fn part2() -> String {
    calculate_ascii_hash(common::read_file("day10/input.txt"))
}

