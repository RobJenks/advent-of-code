use std::collections::HashMap;
use std::iter::Iterator;
use itertools::Itertools;
use crate::common::float::{F64, StandardFloat};
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
    solve_stabilize(&parse_input("src/day24/problem-input.txt"), 10).sum()
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

fn eq_eps(x: f64, y: f64) -> bool {
    (x - y).abs() < (1e-10 * (x.abs() + y.abs()))
}

#[cfg(test)]
fn veq_eps(x: &Vec3<F64>, y: &Vec3<F64>) -> bool {
    x.data.iter().zip(y.data.iter())
        .all(|(v0, v1)| eq_eps(**v0, **v1))
}

fn build_eq_system(particles: &Vec<Particle>, sample_ix: usize) -> EqSystem {
    let p = &particles[sample_ix..sample_ix+3];
    let f = |sf: StandardFloat<f64>| *sf;

    let sys_a: [[f64; 10]; 10] = [
        /* eq1: particles 0 and 1, x and y */
        [f(p[0].velocity.y()-p[1].velocity.y()), f(p[1].velocity.x()-p[0].velocity.x()), 0.0,
         f(p[1].pos.y()-p[0].pos.y()), f(p[0].pos.x()-p[1].pos.x()), 0.0, 0.0, 0.0, 0.0, 0.0].map(f64::from),
        /* eq2: particles 0 and 2, x and y */
        [f(p[0].velocity.y()-p[2].velocity.y()), f(p[2].velocity.x()-p[0].velocity.x()), 0.0,
         f(p[2].pos.y()-p[0].pos.y()), f(p[0].pos.x()-p[2].pos.x()), 0.0, 0.0, 0.0, 0.0, 0.0].map(f64::from),
        /* eq3: particles 0 and 1, x and z */
        [f(p[0].velocity.z()-p[1].velocity.z()), 0.0, f(p[1].velocity.x()-p[0].velocity.x()),
         f(p[1].pos.z()-p[0].pos.z()), 0.0, f(p[0].pos.x()-p[1].pos.x()), 0.0, 0.0, 0.0, 0.0].map(f64::from),
        /* eq4: particles 0 and 2, x and z */
        [f(p[0].velocity.z()-p[2].velocity.z()), 0.0, f(p[2].velocity.x()-p[0].velocity.x()),
         f(p[2].pos.z()-p[0].pos.z()), 0.0, f(p[0].pos.x()-p[2].pos.x()), 0.0, 0.0, 0.0, 0.0].map(f64::from),
        /* eq5: particles 0 and 1, y and z */
        [0.0, f(p[0].velocity.z()-p[1].velocity.z()), f(p[1].velocity.y()-p[0].velocity.y()),
         0.0, f(p[1].pos.z()-p[0].pos.z()), f(p[0].pos.y()-p[1].pos.y()), 0.0, 0.0, 0.0, 0.0].map(f64::from),
        /* eq6: particles 0 and 2, y and z */
        [0.0, f(p[0].velocity.z()-p[2].velocity.z()), f(p[2].velocity.y()-p[0].velocity.y()),
         0.0, f(p[2].pos.z()-p[0].pos.z()), f(p[0].pos.y()-p[2].pos.y()), 0.0, 0.0, 0.0, 0.0].map(f64::from),

        [0.0; 10], [0.0; 10], [0.0; 10], [0.0; 10]
    ];

    let sys_c: [f64; 10] = [
        /* eq1: lines 0 and 1, x and y */
        f(p[1].pos.y()*p[1].velocity.x() - p[1].pos.x()*p[1].velocity.y()
            - p[0].pos.y()*p[0].velocity.x() + p[0].pos.x()*p[0].velocity.y()),
        /* eq2: lines 0 and 2, x and y */
        f(p[2].pos.y()*p[2].velocity.x() - p[2].pos.x()*p[2].velocity.y()
            - p[0].pos.y()*p[0].velocity.x() + p[0].pos.x()*p[0].velocity.y()),
        /* eq3: lines 0 and 1, x and z */
        f(p[1].pos.z()*p[1].velocity.x() - p[1].pos.x()*p[1].velocity.z()
            - p[0].pos.z()*p[0].velocity.x() + p[0].pos.x()*p[0].velocity.z()),
        /* eq4: lines 0 and 2, x and z */
        f(p[2].pos.z()*p[2].velocity.x() - p[2].pos.x()*p[2].velocity.z()
            - p[0].pos.z()*p[0].velocity.x() + p[0].pos.x()*p[0].velocity.z()),
        /* eq5: lines 0 and 1, y and z */
        f(p[1].pos.z()*p[1].velocity.y() - p[1].pos.y()*p[1].velocity.z()
            - p[0].pos.z()*p[0].velocity.y() + p[0].pos.y()*p[0].velocity.z()),
        /* eq6: lines 0 and 2, y and z */
        f(p[2].pos.z()*p[2].velocity.y() - p[2].pos.y()*p[2].velocity.z()
            - p[0].pos.z()*p[0].velocity.y() + p[0].pos.y()*p[0].velocity.z()),

        0.0, 0.0, 0.0, 0.0
    ];

    EqSystem { a: sys_a, c: sys_c, d: 6 }
}

fn solve(system: &EqSystem) -> Result<Particle, String> {
    let mut sys = system.clone();
    for i in 0..system.d {
        let mut highest = 0usize;
        let mut max = 0f64;

        for j in i..system.d {
            if sys.a[j][i].abs() > max {
                highest = j;
                max = sys.a[j][i];
            }
        }

        if eq_eps(max, 0.0) { return Err(String::from("No nonzero start")); }
        (sys.c[i], sys.c[highest]) = (sys.c[highest], sys.c[i]);

        for j in 0..sys.d {
            (sys.a[i][j], sys.a[highest][j]) = (sys.a[highest][j], sys.a[i][j]);
        }

        // Reduce
        for j in (i+1)..sys.d {
            let r = sys.a[j][i] / sys.a[i][i];
            for k in i..sys.d {
                sys.a[j][k] -= r * sys.a[i][k];
            }
            sys.c[j] -= r * sys.c[i];
        }
    }

    // Back substitution
    let mut res = [0f64; 6];
    for i in (0..sys.d).rev() {
        res[i] = sys.c[i];
        for j in (i+1)..sys.d {
            res[i] -= sys.a[i][j] * res[j];
        }
        res[i] /= sys.a[i][i];
    }

    let sf = res.iter().map(|&f| f.into()).collect::<Vec<StandardFloat<f64>>>();
    Ok(Particle::new(Vec3::new_from_slice(&sf[0..3]), Vec3::new_from_slice(&sf[3..6])))
}

fn solve_stabilize(particles: &Vec<Particle>, iterations: usize) -> Vec3<usize> {
  (0..iterations)
        .map(|i| build_eq_system(particles, i))
        .map(|sys| match solve(&sys) {
            Ok(p) => p.pos,
            Err(e) => panic!("Stabilized solution failed: {}", e)
        })
      .filter(|v| !v.data.iter().any(|x| x.is_nan()))
      .map(|v| Vec3::<usize>::new_from_slice(&v.data.iter().map(|x| (*x).round() as usize).collect_vec()))

      .fold(HashMap::<Vec3<_>, usize>::new(), |mut acc, x| {
          *acc.entry(x).or_insert(0) += 1;
          acc
      })

      .iter()
      .max_by(|a, b| a.1.cmp(&b.1))
      .unwrap_or_else(|| panic!("Could not determine stable solution")).0.clone()
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

#[derive(Clone)]
struct EqSystem {
    a: [[f64; 10]; 10],
    c: [f64; 10],
    d: usize
}


#[cfg(test)]
mod tests {
    use crate::common::float::F64;
    use crate::common::vec::Vec3;
    use crate::day24::{build_eq_system, count_intersections, parse_input, part1, part2, solve, solve_stabilize, veq_eps};

    #[test]
    fn test_count_intersections() {
        assert_eq!(count_intersections(&parse_input("src/day24/test-input-1.txt"), (F64::from(7.0), F64::from(27.0))), 2)
    }

    #[test]
    fn test_solve_system_of_eq() {
        match solve(&build_eq_system(&parse_input("src/day24/test-input-1.txt"), 0)) {
            Ok(p) => {
                assert!(veq_eps(&p.pos, &Vec3::<F64>::new(24.0.into(), 13.0.into(), 10.0.into())));
                assert!(veq_eps(&p.velocity, &Vec3::<F64>::new((-3.0).into(), 1.0.into(), 2.0.into())));
            },
            Err(e) => panic!("Failed to solve system: {}", e)
        }
    }

    #[test]
    fn test_solve_stabilize() {
        assert_eq!(
            &solve_stabilize(&parse_input("src/day24/test-input-1.txt"), 3),
            &Vec3::<usize>::new(24, 13, 10)
        );
    }

    #[test]
    fn test_part1() {
        assert_eq!(part1(), 15558);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(), 765636044333842);
    }
}
