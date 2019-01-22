pub fn run() {
    println!("Part 1 result: {}", part1());
}

fn part1() -> u32 {
    common::read_file("day4/input.txt")
        .split("\n")
        .map(|x| x.split_whitespace().collect::<Vec<&str>>())
        .map(|vec| *&vec[0..vec.len() - 1].iter().enumerate()        // Iter words [0 n-1)   [i.e. skip last]
            .all(|(i, x)| !(&vec[i + 1..].contains(x)).clone())
        )
        .filter(|pr| *pr)
        .count() as u32
}

