use std::collections::hash_map::HashMap;
use common::vec3::Vec3;

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> usize {
    // Given a sufficiently-long time, particle with the lowest acceleration will remain the closest to origin
    parse_input(common::read_file("day20/input.txt"))
        .iter().enumerate()
        .map(|(i, p)|(i, p.acc.mag()))
        .min_by(|(_, a0),(_, a1)| a0.cmp(a1))
        .unwrap().0
}

fn part2() -> usize {
    let mut points = parse_input(common::read_file("day20/input.txt"));
    let n = points.len() as isize;

    const STEADY_STATE : usize = 1_000; // Tuned down based on actual execution results
    let mut steady = (0usize, 0usize); // { active#, cycles }

    let mut positions : HashMap<Vec3<isize>, isize> = HashMap::new();
    while steady.1 < STEADY_STATE {
        let active = points.iter_mut()
            .filter(|p| p.active)
            .map(|p| p.simulate())
            .count();

        steady = if active == steady.0 { (steady.0, steady.1 + 1) } else { (active, 0) };

        positions.clear();

        for i in 0..n {
            if !points[i as usize].active { continue }

            let existing = positions.entry(points[i as usize].pos.clone()).or_insert(i);
            if *existing != i {
                points[*existing as usize].active = false;
                points[i as usize].active = false;
            }
        }
    }

    steady.0
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
    active: bool,
    pos: Vec3<isize>,
    vel: Vec3<isize>,
    acc: Vec3<isize>
}

impl Point {
    fn new(pos: &Vec3<isize>, vel: &Vec3<isize>, acc: &Vec3<isize>) -> Self { Self { active: true, pos: pos.clone(), vel: vel.clone(), acc: acc.clone() } }

    // Simulate the particle and return its new position this cycle
    fn simulate(&mut self) -> &Vec3<isize> {
        self.vel = &self.vel + &self.acc;
        self.pos = &self.pos + &self.vel;
        &self.pos
    }
}