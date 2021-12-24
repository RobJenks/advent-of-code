use crate::common::grid::Grid;
use crate::common::vec2::Vec2;
use crate::common;
use itertools::Itertools;

pub fn run() {
    println!("Part 1 result: {}", part1());
}

fn part1() -> usize {
    let lines = parse_input(common::read_file("src/day5/problem-input.txt"));
    let mut grid = Grid::<u32>::new(upper_bounds(&lines), &0);

    record_perpendicular_lines(&mut grid, &lines);

    grid.raw_data().iter()
        .filter(|&x| *x > 1u32)
        .count()
}

fn record_perpendicular_lines(grid: &mut Grid<u32>, lines: &Vec<Line>) {
    lines.iter()
        .filter(|&x| x.is_perpendicular())
        .for_each(|line| line.calc_pixels().iter()
            .for_each(|px| grid.apply_at_coord(px, |x| x+1)))
}

fn parse_input(input: String) -> Vec<Line> {
    input.lines()
        .map(|l| l
            .split(" -> ")
            .collect_tuple::<(&str, &str)>()
            .expect("Invalid input line"))
        .map(|(p0,p1)| Line::new(parse_vec(p0), parse_vec(p1)))
        .collect()
}

fn parse_vec(s: &str) -> Vec2<usize> {
    let comp = s.split(",")
        .collect_tuple::<(&str, &str)>()
        .expect("Invalid line vertex");

    Vec2::new(
        comp.0.parse::<usize>().expect("Invalid component"),
        comp.1.parse::<usize>().expect("Invalid component"))
}

fn upper_bounds(lines: &Vec<Line>) -> Vec2<usize> {
    lines.iter()
        .fold(Vec2::new(0, 0), |max, line| Vec2::new(
            max.x.max(line.start.x.max(line.end.x)),
            max.y.max(line.start.y.max(line.end.y))))
}

struct Line {
    start: Vec2<usize>,
    end: Vec2<usize>,
}

impl Line {
    pub fn new(start: Vec2<usize>, end: Vec2<usize>) -> Self {
        Self { start, end }
    }

    pub fn get_start(&self) -> Vec2<usize> { self.start.clone() }
    pub fn get_end(&self) -> Vec2<usize> { self.end.clone() }

    pub fn is_horizontal(&self) -> bool { self.start.y == self.end.y }
    pub fn is_vertical(&self) -> bool { self.start.x == self.end.x }

    pub fn is_perpendicular(&self) -> bool {
        self.is_horizontal() || self.is_vertical()
    }

    fn ordered_xy(&self) -> (&Vec2<usize>, &Vec2<usize>) {
        if self.start.x <= self.end.x { (&self.start, &self.end ) } else { (&self.end, &self.start ) }
    }

    fn ordered_yx(&self) -> (&Vec2<usize>, &Vec2<usize>) {
        if self.start.y <= self.end.y { (&self.start, &self.end ) } else { (&self.end, &self.start ) }
    }

    pub fn calc_pixels(&self) -> Vec<Vec2<usize>> {
        if self.is_horizontal() {
            let (start, end) = self.ordered_xy();
            (start.x..=end.x).map(|x| Vec2::new(x, start.y)).collect()
        }
        else if self.is_vertical() {
            let (start, end) = self.ordered_yx();
            (start.y..=end.y).map(|y| Vec2::new(start.x, y)).collect()
        }
        else {
            panic!("Not supported");
        }
    }

}

