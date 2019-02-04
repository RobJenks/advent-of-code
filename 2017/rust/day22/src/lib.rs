use common::vec2::Vec2;

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> usize {
    let mut grid = Grid::new(parse_input(common::read_file("day22/input.txt")), Vec2::<isize>{ x: 1001, y: 1001 });
    let virus = grid.spawn_virus(Version::V1, Vec2::<isize> { x: 0, y: 0 }, Direction::Up);

    virus.take(10_000).filter(|x| *x == State::Infected).count()
}

fn part2() -> usize {
    let mut grid = Grid::new(parse_input(common::read_file("day22/input.txt")), Vec2::<isize>{ x: 1001, y: 1001 });
    let virus = grid.spawn_virus(Version::V2, Vec2::<isize> { x: 0, y: 0 }, Direction::Up);

    virus.take(10_000_000).filter(|x| *x == State::Infected).count()
}



#[derive(PartialEq, Debug, Clone, Copy)]
pub enum State { Clean, Weakened, Infected, Flagged }

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum Version { V1, V2 }

#[derive(Debug)]
pub struct Grid {
    data: Vec<State>,
    size: Vec2<isize>,
    count: usize
}

#[derive(Debug)]
pub struct Virus <'a> {
    grid: &'a mut Grid,
    pos: Vec2<isize>,
    dir: Direction,
    version: Version
}

impl Grid {
    pub fn new(mut input: Vec<Vec<State>>, size: Vec2<isize>) -> Self {
        let count = (size.x * size.y) as usize;
        let input_size = Vec2::<isize> { x: input[0].len() as isize, y: input.len() as isize };
        let buffer = &(&size - &input_size) / &Vec2::<isize>{x:2,y:2};

        let data = &mut vec![State::Clean; (buffer.y * size.x) as usize];
        input.iter_mut().for_each(|v| {
            data.append(&mut vec![State::Clean; buffer.x as usize]);
            data.append(v);
            data.append(&mut vec![State::Clean; buffer.x as usize]);
        });
        data.append(&mut vec![State::Clean; (buffer.y * size.x) as usize]);

        Self { data: data.clone(), size, count }
    }
    fn valid_pos(&self, pos: &Vec2<isize>) -> bool { pos.x >= 0 && pos.y >= 0 && pos.x < self.size.x && pos.y < self.size.y }
    fn index(&self, pos: &Vec2<isize>) -> usize { ((pos.y * self.size.x) + pos.x) as usize }
    fn get(&self, pos: &Vec2<isize>) -> State { self.data[self.index(&pos)] }
    fn set(&mut self, pos: &Vec2<isize>, value: State) {
        let ix = self.index(pos);
        self.data[ix] = value;
    }
    fn adj(&self, pos: &Vec2<isize>, dir: Direction) -> Vec2<isize> {
        let cell = pos + &direction(&dir);
        if self.valid_pos(&cell) { cell } else { panic!("Position is out of bounds"); }
    }
    pub fn spawn_virus(&mut self, version: Version, pos: Vec2<isize>, dir: Direction) -> Virus {
        let virus_pos = &pos + &(&self.size / &Vec2::<isize>{x: 2, y: 2});
        Virus { grid: self, pos: virus_pos, dir, version }
    }

    pub fn str_all(&self) -> String { self.str(&Vec2::<isize>{x: 0, y: 0}, &self.size) }
    pub fn str(&self, min: &Vec2<isize>, max: &Vec2<isize>) -> String {
        let mut s = String::new();
        let mut pos = Vec2::<isize> { x: 0, y: 0 };
        for i in 0..self.count {
            if pos.x >= min.x && pos.y < max.x && pos.y >= min.y && pos.y < max.y {
                s += state_str(&self.data[i]);
            }
            pos.x += 1;
            if pos.x == self.size.x {
                pos.x = 0;
                pos.y += 1;
                s += "\n";
            }
        }
        s
    }
}

impl <'a> Virus <'a> {
    fn _grid_str_all(&self) -> String { self.grid.str_all() }

    fn state_transition(&self, state: &State) -> State {
        match state {
            State::Clean if self.version == Version::V1 => State::Infected,
            State::Clean if self.version == Version::V2 => State::Weakened,
            State::Weakened => State::Infected,
            State::Infected if self.version == Version::V1 => State::Clean,
            State::Infected if self.version == Version::V2 => State::Flagged,
            State::Flagged => State::Clean,
            _ => panic!("Unsupported state transition")
        }
    }

    fn turn_direction(&self, state: &State) -> Rotation {
        match state {
            State::Clean => Rotation::Rotate270,
            State::Weakened => Rotation::Rotate0,
            State::Infected => Rotation::Rotate90,
            State::Flagged => Rotation::Rotate180
        }
    }
}

impl <'a> Iterator for Virus <'a> {
    type Item = State;           // The state applied to the current cell in this cycle
    fn next(&mut self) -> Option<Self::Item> {
        let state = self.grid.get(&self.pos);
        let new_state = self.state_transition(&state);

        self.dir = rotate(self.dir, self.turn_direction(&state));
        self.grid.set(&self.pos, new_state);
        self.pos = self.grid.adj(&self.pos, self.dir);

        Some(new_state)
    }
}

fn parse_input(input: String) -> Vec<Vec<State>> {
    input.split("\n")
        .map(|x| x.chars()
            .map(|x| if x == '#' { State::Infected } else { State::Clean })
            .collect::<Vec<State>>()
        )
        .collect::<Vec<Vec<State>>>()
}


#[derive(Clone, Copy, Debug)]
pub enum Direction { Left, Up, Right, Down }

pub fn direction(dir: &Direction) -> Vec2<isize> {
    match dir {
        Direction::Left => Vec2::new(-1, 0),
        Direction::Up => Vec2::new(0, -1),
        Direction::Right => Vec2::new(1, 0),
        Direction::Down => Vec2::new(0, 1)
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Rotation { Rotate0, Rotate90, Rotate180, Rotate270 }

const ROTATIONS : [[Direction; 4]; 4] = [
    [Direction::Left, Direction::Up, Direction::Right, Direction::Down],
    [Direction::Up, Direction::Right, Direction::Down, Direction::Left],
    [Direction::Right, Direction::Down, Direction::Left, Direction::Up],
    [Direction::Down, Direction::Left, Direction::Up, Direction::Right]
];

pub fn rotate(dir: Direction, rot: Rotation) -> Direction { ROTATIONS[dir as usize][rot as usize] }

fn state_str(state: &State) -> &str {
    match state {
        State::Clean => ".",
        State::Weakened => "W",
        State::Infected => "#",
        State::Flagged => "F"
    }
}


#[cfg(test)]
mod tests {
    use super::{Grid, Vec2, Direction, Version, State, parse_input};

    #[test]
    fn test_transitions() {
        let mut grid = Grid::new(parse_input("..#\n#..\n...".to_string()), Vec2::<isize>{ x: 9, y: 9 });
        let mut virus = grid.spawn_virus(Version::V1, Vec2::<isize> { x: 0, y: 0 }, Direction::Up);

        let expected = vec![true, false, true, true, true, true, false];
        expected.iter().for_each(|x| assert_eq!(virus.next().unwrap() == State::Infected, *x));
    }

    #[test]
    fn test_spread() {
        let mut grid = Grid::new(parse_input("..#\n#..\n...".to_string()), Vec2::<isize>{ x: 101, y: 101 });
        let mut virus = grid.spawn_virus(Version::V1, Vec2::<isize> { x: 0, y: 0 }, Direction::Up);

        assert_eq!(virus.by_ref().take(7).filter(|x| *x == State::Infected).count(), 5);
        assert_eq!(virus.by_ref().take(70).filter(|x| *x == State::Infected).count(), 41);
    }

    #[test]
    fn test_evolved() {
        let mut grid = Grid::new(parse_input("..#\n#..\n...".to_string()), Vec2::<isize>{ x: 101, y: 101 });
        let mut virus = grid.spawn_virus(Version::V2, Vec2::<isize> { x: 0, y: 0 }, Direction::Up);

        assert_eq!(virus.by_ref().take(100).filter(|x| *x == State::Infected).count(), 26);
    }

}