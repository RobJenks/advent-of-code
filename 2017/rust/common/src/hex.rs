#[derive(Clone)]
pub struct Hex {
    coords: Vec<i32>
}

#[derive(Clone)]
pub enum HexDir {
    N, NE, SE, S, SW, NW
}

impl Hex {
    pub fn new(q: i32, r: i32, s: i32) -> Self { Self { coords: vec![q, r, s] } }
    pub fn origin() -> Self { Self::new(0, 0, 0) }

    pub fn q(&self) -> i32 { self.coords[0] }
    pub fn r(&self) -> i32 { self.coords[1] }
    pub fn s(&self) -> i32 { self.coords[2] }

    pub fn mag(&self) -> i32 { self.coords.iter().map(|x| x.abs()).sum::<i32>() / 2 }
    pub fn dist(&self, other: &Hex) -> i32 { (self - other).mag() }

    pub fn neighbour(&self, dir: HexDir) -> Hex { self + &Hex::direction(dir) }

    pub fn direction(dir: HexDir) -> Hex {
        match dir {
            HexDir::N =>  Hex::new(0, 1, -1),
            HexDir::NE => Hex::new(1, 0, -1),
            HexDir::SE => Hex::new(1, -1, 0),
            HexDir::S =>  Hex::new(0, -1, 1),
            HexDir::SW => Hex::new(-1, 0, 1),
            HexDir::NW => Hex::new(-1, 1, 0)
        }
    }

    pub fn follow_path(&self, directions: &Vec<HexDir>) -> Hex {
        directions.iter().fold(self.clone(), |pos, d| pos.neighbour(d.clone()))
    }
}

impl std::ops::Add for &Hex {
    type Output = Hex;
    fn add(self, rhs: Self) -> Self::Output {
        Self::Output { coords: self.coords.iter().zip(rhs.coords.iter()).map(|(x, y)| x + y).collect() }
    }
}

impl std::ops::Sub for &Hex {
    type Output = Hex;
    fn sub(self, rhs: Self) -> Self::Output {
        Self::Output { coords: self.coords.iter().zip(rhs.coords.iter()).map(|(x, y)| x - y).collect() }
    }
}

impl HexDir {
    pub fn from_str(s: &str) -> HexDir {
        match s {
            "n" => HexDir::N,
            "ne" => HexDir::NE,
            "se" => HexDir::SE,
            "s" => HexDir::S,
            "sw" => HexDir::SW,
            "nw" => HexDir::NW,
            _ => panic!("Invalid hex direction")
        }
    }
}