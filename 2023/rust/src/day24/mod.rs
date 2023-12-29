use std::iter::Iterator;
use itertools::Itertools;
use crate::common::float::F64;
use crate::common::vec::Vec3;
use super::common;

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> usize {
    count_intersections(&parse_input("src/day24/problem-input.txt"),
                        (F64::from(200000000000000.0), F64::from(400000000000000.0)))
}

fn part2() -> usize {
 12
}

fn count_intersections(particles: &Vec<Particle>, bounds: (F64, F64)) -> usize {
    // Solve as linear eq - precalculate mx + c
    let data = particles.iter()
        .map(|p| {
            let m = p.velocity.y() / p.velocity.x();
            let c = p.pos.y() - (m * p.pos.x());
            Data::new(p, m, c)
        })
        .collect_vec();

    data.iter().enumerate()
        .map(|(ix, p0)| data.iter().skip(ix + 1)
            .filter(|&p1| test_intersection(p0, p1, &bounds))
            .count())
        .sum()
}

fn test_intersection(p0: &Data, p1: &Data, bounds: &(F64, F64)) -> bool {
    if p1.m - p0.m == 0.0 { return false } // Parallel

    let x = (p0.c - p1.c) / (p1.m - p0.m);
    let y = p0.m * x + p0.c;    // mx + c for potential intersection point

    if x < bounds.0 || x > bounds.1 || y < bounds.0 || y > bounds.1 { return false }

    // Make sure intersection point is the right side of each line
    (p0.particle.velocity.y() > 0.0 && y > p0.particle.pos.y() ||
     p0.particle.velocity.y() < 0.0 && y < p0.particle.pos.y())
    &&
    (p1.particle.velocity.y() > 0.0 && y > p1.particle.pos.y() ||
     p1.particle.velocity.y() < 0.0 && y < p1.particle.pos.y())
}

fn parse_input(file: &str) -> Vec<Particle> {
    common::read_file(file).lines()
        .map(|line| line.split_once("@").unwrap_or_else(|| panic!("Invalid input format")))
        .map(|(pos, vel)| (
            pos.split(",").map(|s| s.trim().parse::<F64>().unwrap_or_else(|e| panic!("Invalid pos ({})", e)))
                .collect_tuple::<(F64, F64, F64)>().unwrap_or_else(|| panic!("Invalid pos")),
            vel.split(",").map(|s| s.trim().parse::<F64>().unwrap_or_else(|e| panic!("Invalid velocity ({})", e)))
                .collect_tuple::<(F64, F64, F64)>().unwrap_or_else(|| panic!("Invalid velocity"))))
        .map(|(pos, vel)| Particle::new(Vec3::new(pos.0, pos.1, pos.2), Vec3::new(vel.0, vel.1, vel.2)))
        .collect_vec()
}

struct Particle {
    pos: Vec3<F64>,
    velocity: Vec3<F64>
}
impl Particle {
    pub fn new(pos: Vec3<F64>, velocity: Vec3<F64>) -> Self {
        Self { pos, velocity }
    }
}

struct Data<'a> {
    particle: &'a Particle,
    m: F64,
    c: F64
}
impl<'a> Data<'a> {
    pub fn new(particle: &'a Particle, m: F64, c: F64) -> Self {
        Self { particle, m, c }
    }
}


#[cfg(test)]
mod tests {
    use crate::common::float::F64;
    use crate::day24::{count_intersections, parse_input, part1, part2};

    #[test]
    fn test_count_intersections() {
        assert_eq!(count_intersections(&parse_input("src/day24/test-input-1.txt"), (F64::from(7.0), F64::from(27.0))), 2)
    }

    #[test]
    fn test_part1() {
        assert_eq!(part1(), 15558);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(), 12);
    }

}
