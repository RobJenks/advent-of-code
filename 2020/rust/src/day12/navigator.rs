use crate::common::vec2::Vec2;
use crate::day12::commands::*;
use std::ops::Add;

const INITIAL_WAYPOINT: Vec2<i32> = Vec2 { x: 10, y: 1 };

pub struct Navigator {
    mode: NavigatorMode,
    pos: Vec2<i32>,
    dir: Direction,
    waypoint: Vec2<i32>
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum NavigatorMode {
    DirectNavigation,
    WaypointNavigation
}

impl Navigator {
    pub fn new(mode: NavigatorMode) -> Self {
        Self { mode, pos: Vec2::new(0, 0), dir: Direction::E, waypoint: INITIAL_WAYPOINT }
    }

    fn new_with_pos(&self, pos: Vec2<i32>) -> Self {
        Self { mode: self.mode, pos, dir: self.dir, waypoint: self.waypoint }
    }

    fn new_with_dir(&self, dir: Direction) -> Self {
        Self { mode: self.mode, pos: self.pos, dir, waypoint: self.waypoint }
    }

    fn new_with_waypoint(&self, waypoint: Vec2<i32>) -> Self {
        Self { mode: self.mode, pos: self.pos, dir: self.dir, waypoint }
    }

    pub fn execute_command(&self, cmd: &Command) -> Navigator {
        match self.mode {
            NavigatorMode::DirectNavigation => self.execute_direct_command(cmd),
            NavigatorMode::WaypointNavigation => self.execute_waypoint_command(cmd)
        }
    }

    pub fn execute_direct_command(&self, cmd: &Command) -> Navigator {
        match cmd {
            &Command::MoveCompass(direction, n) => self.new_with_pos(self.pos.add(Direction::move_by(direction, n))),
            &Command::MoveHeading(n) => self.new_with_pos(self.pos.add(Direction::move_by(self.dir, n))),
            &Command::Turn(turn, degrees) => self.new_with_dir(Direction::rotate(self.dir, turn, degrees))
        }
    }

    pub fn execute_waypoint_command(&self, cmd: &Command) -> Navigator {
        match cmd {
            &Command::MoveCompass(direction, n) => self.new_with_waypoint(self.waypoint.add(Direction::move_by(direction, n))),
            &Command::MoveHeading(n) => self.new_with_pos(self.pos.add(self.waypoint.scalar_mul(n))),
            &Command::Turn(turn, degrees) => self.new_with_waypoint(Navigator::rotate_about_origin(&self.waypoint, turn, degrees))
        }
    }

    pub fn manhattan_dist_from(&self, point: &Vec2<i32>) -> u32 {
        self.pos.manhattan_dist(point) as u32
    }

    fn rotate_about_origin(point: &Vec2<i32>, turn: TurnDirection, degrees: i32) -> Vec2<i32> {
        let compass_turns = TurnDirection::compass_dir_turns(degrees)
            * if turn == TurnDirection::L { -1 } else { 1 };

        match (compass_turns + 4) % 4 {
            0 => point.clone(),
            1 => Vec2::new(point.y, -point.x),
            2 => Vec2::new(-point.x, -point.y),
            3 => Vec2::new(-point.y, point.x),
            _ => panic!("Unrecognised compass turn: {}", compass_turns)
        }
    }
}
