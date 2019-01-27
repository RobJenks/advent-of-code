pub fn run() {
    println!("Part 1 result: {}", part1());
}

fn part1() -> usize {
    let mut progs = parse_input(common::read_file("day12/input.txt"));
    assign_groups(&mut progs);

    let g0 = progs[0].group.unwrap();
    progs.iter().filter(|x| x.group.unwrap() == g0).count()
}

fn parse_input(input: String) -> Vec<Prog> {
    input.split("\n")
        .map(|x| x.split(" ")
            .map(|x| x.replace(",", ""))
            .collect::<Vec<String>>()
        )
        .map(|vec| Prog::new((&vec[2..]).iter()
            .map(|x| x.parse::<usize>().unwrap())
            .collect())
        )
        .collect::<Vec<Prog>>()
}

fn assign_groups(progs: &mut Vec<Prog>) {
    let mut group = 0u32;

    for i in 0..progs.len().clone() {
        if progs[i].group.is_none() {
            progs[i].group = Some(group);

            let mut eval = progs[i].links.clone();
            while !eval.is_empty() {
                let other = eval.pop().unwrap();
                if progs[other].group.is_some() {
                    if progs[other].group.unwrap() != group { panic!("Intersecting separate group"); }
                    continue;
                }

                progs[other].group = Some(group);
                progs[other].links.iter().for_each(|x| eval.push(*x));
            }

            group += 1;
        }
    }
}

#[derive(Debug)]
struct Prog {
    links: Vec<usize>,
    group: Option<u32>
}

impl Prog {
    fn new(links: Vec<usize>) -> Self { Self { links, group: None } }
}
