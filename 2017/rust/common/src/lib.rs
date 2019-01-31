pub mod hex;
pub mod hash;

pub fn read_file(path : &str) -> String {
    match std::fs::read_to_string(path) {
        Err(e) => panic!("Failed to read \"{}\": {}", path, e),
        Ok(res) => res
    }
}

#[derive(Debug, Clone)]
pub struct Vec2<T> {
    pub x : T,
    pub y : T
}

impl <T> Vec2<T> {
    pub fn new(x: T, y: T) -> Self { Self { x, y } }
}

impl Vec2<i32> {
    pub fn manhattan(self, rhs : &Vec2<i32>) -> i32 { (self.x - rhs.x).abs() + (self.y - rhs.y).abs() }
}

impl <T> std::ops::Add<&Vec2<T>> for &Vec2<T>
    where T: std::ops::Add<Output = T> + Copy {

    type Output = Vec2<T>;
    fn add(self, rhs: &Vec2<T>) -> Vec2<T> { Vec2::<T>{ x: (self.x + rhs.x), y: (self.y + rhs.y) } }
}

impl <T> std::ops::Sub<&Vec2<T>> for &Vec2<T>
    where T: std::ops::Sub<Output = T> + Copy {

    type Output = Vec2<T>;
    fn sub(self, rhs: &Vec2<T>) -> Vec2<T> { Vec2::<T>{ x: (self.x - rhs.x), y: (self.y - rhs.y) } }
}

impl <T> std::ops::Mul<&Vec2<T>> for &Vec2<T>
    where T: std::ops::Mul<Output = T> + Copy {

    type Output = Vec2<T>;
    fn mul(self, rhs: &Vec2<T>) -> Vec2<T> { Vec2::<T>{ x: (self.x * rhs.x), y: (self.y * rhs.y) } }
}

impl <T> std::ops::Div<&Vec2<T>> for &Vec2<T>
    where T: std::ops::Div<Output = T> + Copy {

    type Output = Vec2<T>;
    fn div(self, rhs: &Vec2<T>) -> Vec2<T> { Vec2::<T>{ x: (self.x / rhs.x), y: (self.y / rhs.y) } }
}

impl <T> std::cmp::Eq for Vec2<T> where T: std::cmp::Eq { }
impl <T> std::cmp::PartialEq for Vec2<T>
    where T: std::cmp::PartialEq {
    fn eq(&self, other: &Vec2<T>) -> bool { (self.x == other.x) && (self.y == other.y) }
}
