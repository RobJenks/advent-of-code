use crate::common::vec2::Vec2;

const DIRECTIONS: &[Direction] = &[Direction::N, Direction::E, Direction::S, Direction::W];

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Direction {
    N, E, S, W
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum TurnDirection {
    L, R
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Command {
    MoveCompass(Direction, i32),
    MoveHeading(i32),
    Turn(TurnDirection, i32)
}

impl Command {
    pub fn new(cmd: &str) -> Command {
        match (cmd.chars().nth(0).unwrap(), &cmd[1usize..]) {
            ('N', n) => Command::MoveCompass(Direction::N, n.parse().unwrap()),
            ('E', n) => Command::MoveCompass(Direction::E, n.parse().unwrap()),
            ('S', n) => Command::MoveCompass(Direction::S, n.parse().unwrap()),
            ('W', n) => Command::MoveCompass(Direction::W, n.parse().unwrap()),
            ('L', n) => Command::Turn(TurnDirection::L, n.parse().unwrap()),
            ('R', n) => Command::Turn(TurnDirection::R, n.parse().unwrap()),
            ('F', n) => Command::MoveHeading(n.parse().unwrap()),
            _ => panic!("Unrecognised command: {}", cmd)
        }
    }
}

impl Direction {
    pub fn move_by(dir: Direction, n: i32) -> Vec2<i32> {
        match dir {
            Direction::N => Vec2::new(0, n),
            Direction::E => Vec2::new(n, 0),
            Direction::S => Vec2::new(0, -n),
            Direction::W => Vec2::new(-n, 0)
        }
    }

    pub fn rotate(dir: Direction, turn: TurnDirection, degrees: i32) -> Direction {
        let dir_ref = match dir {
            Direction::N => 0,
            Direction::E => 1,
            Direction::S => 2,
            Direction::W => 3
        };

        let compass_turns = TurnDirection::compass_dir_turns(degrees)
            * if turn == TurnDirection::L { -1 } else { 1 };

        DIRECTIONS[((dir_ref + compass_turns + 4) % 4) as usize]
    }
}

impl TurnDirection {
    pub fn compass_dir_turns(degrees: i32) -> i32 {
        assert_eq!((degrees as f32 / 90.0), ((degrees / 90) as f32));
        degrees / 90
    }
}