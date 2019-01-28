use std::collections::hash_map::HashMap;

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> String {
    execute_moves(&create_programs(16), &parse_input(common::read_file("day16/input.txt")))
        .iter().collect::<String>()
}

fn part2() -> String {
    let mut progs = create_programs(16);
    let moves = parse_input(common::read_file("day16/input.txt"));

    let mut history : Vec<Vec<char>> = vec![];
    let mut seen = HashMap::<Vec<char>, usize>::new();

    for i in 0usize.. {
        progs = execute_moves(&progs, &moves);

        match seen.get(&progs) {
            Some(_) => return history[(1_000_000_000 % i)-1].iter().collect::<String>(),
            None => { seen.insert(progs.clone(), i); history.push(progs.clone()); }
        }
    }

    panic!("Incorrectly terminated search");
}


fn create_programs(n: u8) -> Vec<char> {
    (0u8..n).map(|x| char::from(97+x)).collect::<Vec<char>>()
}

fn execute_moves(progs: &Vec<char>, moves: &Vec<Move>) -> Vec<char> {
    let mut vec = progs.clone();
    let n = vec.len();

    moves.iter().for_each(|x| match x {
        Move::Spin{x} => vec = (&vec[(n-x)..]).iter().chain((&vec[0..(n-x)]).iter()).map(|x| *x).collect(),
        Move::Exchange {x, y} => vec.swap(*x, *y),
        Move::Partner {x, y} => {
            let (ix,iy) : (usize,usize) = vec.iter().enumerate()
                .fold((0,0), |(i0,i1),(i,c)| if c == x {(i, i1)} else { if c == y {(i0, i)} else {(i0, i1)}});
            vec.swap(ix, iy);
        }
    });

    vec
}

#[derive(Debug)]
enum Move {
    Spin { x: usize },
    Exchange { x: usize, y: usize },
    Partner { x: char, y: char}
}

fn parse_input(input: String) -> Vec<Move> {
    input.split(",")
        .map(|s| (s, s.chars().collect::<Vec<char>>()))
        .map(|(s, ch)| match s {
            x if ch[0] == 's' => Move::Spin { x: (&x[1..]).parse::<usize>().unwrap() },
            x if ch[0] == 'x' => {
                let comp = (&x[1..]).split("/").collect::<Vec<&str>>();
                Move::Exchange { x: comp[0].parse::<usize>().unwrap(), y: comp[1].parse::<usize>().unwrap() }
            },
            _ if ch[0] == 'p' => Move::Partner {x: ch[1], y: ch[3] },
            _ => panic!("Unknown move type")
        })
        .collect::<Vec<Move>>()
}


#[cfg(test)]
mod tests {
    use super::{parse_input, create_programs, execute_moves};

    #[test]
    fn test_moves() {
        assert_eq!(execute_moves(
            &create_programs(5),
            &parse_input("s1,x3/4,pe/b".to_string())),
                   vec!['b','a','e','d','c']
        )
    }

}