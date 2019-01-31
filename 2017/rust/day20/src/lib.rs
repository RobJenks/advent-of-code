use common::vec3::Vec3;

pub fn run() {
    println!("Part 1 result: {}", part1());
}

fn part1() -> usize {
    // Given a sufficiently-long time, particle with the lowest acceleration will remain the closest to origin
    parse_input(common::read_file("day20/input.txt"))
        .iter().enumerate()
        .map(|(i, p)|(i, p.acc.mag()))
        .min_by(|(_, a0),(_, a1)| a0.cmp(a1))
}


fn parse_input(input: String) -> Vec<Point> {
    input.split("\n")
        .map(|s| s.split(", ").map(|ss| ss.trim())
            .map(|s2| (&s2[3..(s2.len()-1)]).split(",")
                .map(|s3| s3.parse::<isize>().unwrap())
                .collect::<Vec<isize>>()
            )
            .map(|vec| Vec3::<isize>::new(vec[0], vec[1], vec[2]))
            .collect::<Vec<Vec3<isize>>>()
        )
        .map(|vv| Point::new(&vv[0], &vv[1], &vv[2]))
        .collect::<Vec<Point>>()
}


#[derive(Clone, Debug)]
struct Point {
    pos: Vec3<isize>,
    vel: Vec3<isize>,
    acc: Vec3<isize>
}

impl Point {
    fn new(pos: &Vec3<isize>, vel: &Vec3<isize>, acc: &Vec3<isize>) -> Self { Self { pos: pos.clone(), vel: vel.clone(), acc: acc.clone() } }
}