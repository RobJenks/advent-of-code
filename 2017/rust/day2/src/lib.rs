pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
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

fn part2() -> u32 {
    let input : Vec<String> = common::read_file("day2/input.txt").split("\n").map(|x| x.to_string()).collect();

    // Split rows and collect/parse into a vec of vec<u32>
    let mut sorted_rows = input.iter()
        .map(|x| x.split_whitespace()
            .map(|x| x.parse::<u32>().unwrap())
            .collect::<Vec<u32>>()
        )
        .collect::<Vec<Vec<u32>>>();

    // Sort each row so we only compare values against possible divisors < than themselves
    sorted_rows.iter_mut()
        .for_each(|vec| vec.sort_unstable());

    // Map each row to its unique divisor result, and return the sum
    sorted_rows.iter()
        .map::<u32,_>(|vec : &Vec<u32>| vec.iter().enumerate()      // for each vec
            .map(move |(i, x)| (x, &vec[..i]))                      // vec[i] -> (vec[i], vec[0..i])
            .map(|(val, others)| others.iter()                                                // for each element in the subset < vec[i]
                .fold(0u32, |res, el| if (val % el) == 0 { val / el } else { res }))  // (vec[i], vec[0..i]) -> u32     (the unique divisor)
            .sum()
        )
        .sum::<u32>()
}





