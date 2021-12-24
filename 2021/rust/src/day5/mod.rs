use crate::common::grid::Grid;
use crate::common::vec2::Vec2;
use crate::common;
use itertools::Itertools;

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> usize {
    process_grid(common::read_file("src/day5/problem-input.txt"), |line| line.is_perpendicular())
}

fn part2() -> usize {
    process_grid(common::read_file("src/day5/problem-input.txt"), |_| true)
}

fn process_grid(input: String, criteria: fn(&Line) -> bool) -> usize {
    let lines = parse_input(input).iter()
        .filter(|&x| criteria(x))
        .cloned()
        .collect();

    let mut grid = Grid::<u32>::new(upper_bounds(&lines), &0);
    record_lines(&mut grid, &lines);

    grid.raw_data().iter()
        .filter(|&x| *x > 1u32)
        .count()
}

fn record_lines(grid: &mut Grid<u32>, lines: &Vec<Line>) {
    lines.iter()
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
        + Vec2::new(1, 1)
}

#[derive(Clone, Eq, PartialEq)]
struct Line {
    start: Vec2<usize>,
    end: Vec2<usize>,
}

impl Line {
    pub fn new(start: Vec2<usize>, end: Vec2<usize>) -> Self {
        Self { start, end }
    }

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
        else /* Is diagonal */ {
            let mut result = vec![];
            let delta = ( if self.end.x > self.start.x { 1 } else { -1 },
                          if self.end.y > self.start.y { 1 } else { -1 });

            let mut px = self.start.clone();
            loop {
                result.push(px);
                if px == self.end { break; }

                px = Vec2::new((px.x as isize + delta.0) as usize, (px.y as isize + delta.1) as usize);
            }

            result
        }
    }
}

#[cfg(test)]
mod test {
    use crate::day5::{part1, part2};

    #[test]
    fn test_part1() {
        assert_eq!(part1(), 7644);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(), 18627);
    }
}

