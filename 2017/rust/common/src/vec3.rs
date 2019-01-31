#[derive(Debug, Clone, Hash)]
pub struct Vec3<T> {
    pub x : T,
    pub y : T,
    pub z : T
}

impl <T> Vec3<T> {
    pub fn new(x: T, y: T, z: T) -> Self { Self { x, y, z } }
}

impl Vec3<isize> {
    pub fn manhattan(self, rhs : &Vec3<isize>) -> isize { (self.x - rhs.x).abs() + (self.y - rhs.y).abs() + (self.z - rhs.z).abs() }
    pub fn mag(&self) -> isize { self.x.abs() + self.y.abs() + self.z.abs() }
}

impl <T> std::ops::Add<&Vec3<T>> for &Vec3<T>
    where T: std::ops::Add<Output = T> + Copy {

    type Output = Vec3<T>;
    fn add(self, rhs: &Vec3<T>) -> Vec3<T> { Vec3::<T>{ x: (self.x + rhs.x), y: (self.y + rhs.y), z: (self.z + rhs.z) } }
}

impl <T> std::ops::Sub<&Vec3<T>> for &Vec3<T>
    where T: std::ops::Sub<Output = T> + Copy {

    type Output = Vec3<T>;
    fn sub(self, rhs: &Vec3<T>) -> Vec3<T> { Vec3::<T>{ x: (self.x - rhs.x), y: (self.y - rhs.y), z: (self.z - rhs.z) } }
}

impl <T> std::ops::Mul<&Vec3<T>> for &Vec3<T>
    where T: std::ops::Mul<Output = T> + Copy {

    type Output = Vec3<T>;
    fn mul(self, rhs: &Vec3<T>) -> Vec3<T> { Vec3::<T>{ x: (self.x * rhs.x), y: (self.y * rhs.y), z: (self.z * rhs.z) } }
}

impl <T> std::ops::Div<&Vec3<T>> for &Vec3<T>
    where T: std::ops::Div<Output = T> + Copy {

    type Output = Vec3<T>;
    fn div(self, rhs: &Vec3<T>) -> Vec3<T> { Vec3::<T>{ x: (self.x / rhs.x), y: (self.y / rhs.y), z: (self.z / rhs.z) } }
}

impl <T> std::cmp::Eq for Vec3<T> where T: std::cmp::Eq { }
impl <T> std::cmp::PartialEq for Vec3<T>
    where T: std::cmp::PartialEq {
    fn eq(&self, other: &Vec3<T>) -> bool { (self.x == other.x) && (self.y == other.y) && (self.z == other.z) }
}
