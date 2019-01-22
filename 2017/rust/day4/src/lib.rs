pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> u32 {
    process_input(|x| x.to_string())
}

fn part2() -> u32 {
    process_input(|x| sort_string(x))
}

fn process_input(modify_word: fn(&str) -> String) -> u32 {
    common::read_file("day4/input.txt")
        .split("\n")
        .map(|x| x.split_whitespace().collect::<Vec<&str>>())
        .map(|vec| vec.iter()
            .map(|x| modify_word(x))
            .collect::<Vec<String>>()
        )
        .map(|vec| *&vec[0..vec.len()-1].iter().enumerate()        // Iter words [0 n-1)   [i.e. skip last]
            .all(|(i, x)| !(&vec[(i+1)..].contains(x)).clone())
        )
        .filter(|pr| *pr)
        .count() as u32

}

fn sort_string(s: &str) -> String {
    let st = s.clone();
    let mut ch = st.chars().collect::<Vec<char>>();
    ch.sort();

    ch.iter().collect::<String>()
}
