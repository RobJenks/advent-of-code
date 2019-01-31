use common::vec2::Vec2;

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> String {
    traverse(&parse_input(common::read_file("day19/input.txt")))
        .iter()
        .collect::<String>()
}

fn part2() -> usize {
    route_length(&parse_input(common::read_file("day19/input.txt")))
}



fn traverse(grid : &Grid) -> Vec<char> {
    // Find entry point
    let pos = Vec2::<i32>::new(grid.data[0].iter().position(|c| *c == '|').unwrap() as i32, 0i32);

    GridIter::new(grid, &pos, &Direction::Down)
        .filter(|(ch, _, _)| ch.is_some())
        .map(|(ch, _, _)| ch.unwrap())
        .collect::<Vec<char>>()
}

fn route_length(grid : &Grid) -> usize {
    let pos = Vec2::<i32>::new(grid.data[0].iter().position(|c| *c == '|').unwrap() as i32, 0i32);
    GridIter::new(grid, &pos, &Direction::Down).count() + 1
}

struct Grid {
    data: Vec<Vec<char>>,
    dimensions: Vec2<i32>
}


impl Grid {
    fn new(data: &Vec<Vec<char>>) -> Self { Self { data: data.clone(), dimensions: Vec2::<i32> { x: data[0].len() as i32, y: data.len() as i32 } } }
    fn get_cell(&self, pos: &Vec2<i32>) -> Option<char> {
        match pos {
            _ if pos.x < 0 || pos.y < 0 || pos.x >= self.dimensions.x || pos.y >= self.dimensions.y => None,
            v => Some(self.data[v.y as usize][v.x as usize])
        }
    }
    fn is_traversable(ch: Option<char>) -> bool {ch.is_some() && ch.unwrap() != ' ' }
    fn is_straight(ch: Option<char>) -> bool { Self::is_traversable(ch) && ch.unwrap() != '+' }
    fn can_traverse(&self, pos: &Vec2<i32>) -> bool { Self::is_traversable(self.get_cell(pos)) }
    fn can_turn_into(&self, pos: &Vec2<i32>) -> bool { Self::is_straight(self.get_cell(pos)) }
}

struct GridIter <'a> {
    grid: &'a Grid,
    pos: Vec2<i32>,
    dir: Direction
}

impl <'a> GridIter <'a> {
    fn new(grid: &'a Grid, start: &Vec2<i32>, start_dir: &Direction) -> Self { Self { grid, pos: start.clone(), dir: start_dir.clone() } }
}

impl <'a> Iterator for GridIter<'a>  {
    type Item = (Option<char>, Vec2<i32>, Direction);
    fn next(&mut self) -> Option<Self::Item> {
        let nxt = &self.pos + &direction(&self.dir);
        if !self.grid.can_traverse(&nxt) {
            let dirs = possible_turns(&self.dir);   // We always have exactly one possible turn
            let turn = dirs.iter()
                .map(|d| (d, self.grid.can_turn_into(&(&self.pos + &direction(d)))))
                .filter(|(_, possible)| *possible)
                .last();

            // We always have exactly one possible turn, unless we are at the end of the line
            if turn.is_none() { return None; }
            self.dir = *turn.unwrap().0;
        }

        self.pos = &self.pos + &direction(&self.dir);
        Some((match self.grid.get_cell(&self.pos) {
            None => panic!("Left the grid area"),
            Some(' ') => panic!("Left the traversable route"),
            Some('-') | Some('|') | Some('+') => None,
            x @ Some(_) => x,
        }, self.pos.clone(), self.dir))
    }
}

#[derive(Clone, Copy)]
enum Direction { Up, Left, Down, Right }

fn direction(dir: &Direction) -> Vec2<i32> {
    match dir {
        Direction::Up => Vec2::new(0, -1),
        Direction::Left => Vec2::new(-1, 0),
        Direction::Right => Vec2::new(1, 0),
        Direction::Down => Vec2::new(0, 1)
    }
}

fn possible_turns(current_direction: &Direction) -> [Direction;2] {
    match *current_direction {
        Direction::Up | Direction::Down => [Direction::Left, Direction::Right],
        Direction::Left | Direction::Right => [Direction::Up, Direction::Down]
    }
}


fn parse_input(input: String) -> Grid {
    Grid::new(&input.split("\n")
        .map(|x| x.chars().collect::<Vec<char>>())
        .collect::<Vec<Vec<char>>>())
}

#[cfg(test)]
mod tests {
    use super::{parse_input, traverse, route_length};

    #[test]
    fn test_traversal() {
        assert_eq!(traverse(&parse_input(common::read_file("tests.txt")))
            .iter()
            .collect::<String>(), "ABCDEF");
    }

    #[test]
    fn test_route_length() {
        assert_eq!(route_length(&parse_input(common::read_file("tests.txt"))), 38);
    }

}