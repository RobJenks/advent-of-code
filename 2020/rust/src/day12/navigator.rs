use crate::common::vec2::Vec2;
use crate::day12::commands::*;
use std::ops::Add;

pub struct Navigator {
    pos: Vec2<i32>,
    dir: Direction,
}

impl Navigator {
    pub fn new() -> Self {
        Self { pos: Vec2::new(0, 0), dir: Direction::E }
    }

    fn new_at(pos: Vec2<i32>, dir: Direction) -> Self {
        Self { pos, dir }
    }

    pub fn execute_command(&self, cmd: &Command) -> Navigator {
        match cmd {
            &Command::MoveCompass(direction, n) => Navigator::new_at(self.pos.add(Direction::move_by(direction, n)), self.dir),
            &Command::MoveHeading(n) => Navigator::new_at(self.pos.add(Direction::move_by(self.dir, n)), self.dir),
            &Command::Turn(turn, degrees) => Navigator::new_at(self.pos, Direction::rotate(self.dir, turn, degrees))
        }
    }

    pub fn manhattan_dist_from(&self, point: &Vec2<i32>) -> u32 {
        self.pos.manhattan_dist(point) as u32
    }

}
