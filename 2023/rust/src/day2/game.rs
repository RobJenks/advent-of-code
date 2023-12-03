use Color::*;

#[derive(Eq, PartialEq, Copy, Clone)]
pub enum Color { Red = 0, Green = 1, Blue = 2 }

impl From<String> for Color {
    fn from(str: String) -> Self {
        match str.to_ascii_uppercase().as_str() {
            "RED" => Red,
            "GREEN" => Green,
            "BLUE" => Blue,
            _ => panic!("Invalid color")
        }
    }
}

pub struct Cubes {
    color : Color,
    count : u32
}

pub struct Round {
    cubes : Vec<Cubes>
}

pub struct Game {
    id : u32,
    rounds : Vec<Round>
}

pub struct Games {
    games : Vec<Game>
}

impl Cubes {
    pub fn new(color : Color, count : u32) -> Self {
        Self { color, count }
    }

    pub fn get_count(&self) -> u32 { self.count }
}

impl Round {
    pub fn new(cubes : Vec<Cubes>) -> Self {
        Self { cubes }
    }

    pub fn is_possible(&self, available: &Vec<Cubes>) -> bool {
        self.cubes.iter()
            .all(|req| available.iter()
                .find(|&avail| avail.color == req.color)
                .map(|avail| req.count <= avail.count)
                .unwrap_or_else(|| false))
    }
}

impl Game {
    pub fn new(id : u32, rounds: Vec<Round>) -> Self {
        Self { id, rounds }
    }

    pub fn get_id(&self) -> u32 { self.id }

    pub fn is_possible(&self, available: &Vec<Cubes>) -> bool {
        self.rounds.iter()
            .all(|round| round.is_possible(available))
    }

    pub fn get_power(&self) -> u32 {
        self.rounds.iter()
            .fold([0, 0, 0], |acc, x| [
                acc[0].max(x.cubes.iter().find(|&c| c.color == Red).map(Cubes::get_count).unwrap_or_else(|| 0)),
                acc[1].max(x.cubes.iter().find(|&c| c.color == Green).map(Cubes::get_count).unwrap_or_else(|| 0)),
                acc[2].max(x.cubes.iter().find(|&c| c.color == Blue).map(Cubes::get_count).unwrap_or_else(|| 0))])

            .iter().product::<u32>()
    }
}

impl Games {
    pub fn new(games: Vec<Game>) -> Self {
        Self { games }
    }

    pub fn iter(&self, pred: fn(&Game) -> bool) -> GameIter<'_> {
        GameIter::new(self, pred)
    }
    pub fn iter_all(&self) -> GameIter<'_> { GameIter::new(self, |_| true) }
}

pub struct GameIter<'a> {
    data: &'a Games,
    pred: fn(&Game) -> bool,
    index: u32
}

impl<'a> GameIter<'a> {
    pub fn new(data: &'a Games, pred: fn(&Game) -> bool) -> Self {
        Self { data, pred, index: 0 }
    }
}

impl<'a> Iterator for GameIter<'a> {
    type Item = &'a Game;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let index = self.index as usize;
            if index >= self.data.games.len() { break None }
            self.index += 1;

            if (self.pred)(&self.data.games[index]) { break Some(&self.data.games[index]) }
        }
    }
}
