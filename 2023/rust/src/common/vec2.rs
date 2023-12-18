use std::fmt::{Display, Formatter};
use std::ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Sub, SubAssign};
use crate::common::num::{Numeric, Zero};

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Debug)]
pub struct Vec2<T>
    where T: Numeric + Copy + Display {

    pub x: T,
    pub y: T
}

impl <T> Vec2<T>
    where T: Numeric + Copy + Display {

    pub fn new(x: T, y: T) -> Self { Self { x, y } }
    pub fn new_uniform(xy: T) -> Self { Self { x: xy, y: xy } }

    pub fn component_min(&self, other: &Vec2<T>) -> Self {
        Self { x: self.x.min(other.x), y: self.y.min(other.y) }
    }
    pub fn component_max(&self, other: &Vec2<T>) -> Self {
        Self { x: self.x.max(other.x), y: self.y.max(other.y) }
    }
}

impl <T> Numeric for Vec2<T>
    where T: Numeric + Copy + Display {
}

impl <T> Add for Vec2<T>
    where T: Numeric + Copy + Display {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self {
            x: self.x + rhs.x,
            y: self.y + rhs.y
        }
    }
}

impl <T> Sub for Vec2<T>
    where T: Numeric + Copy + Display {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self {
            x: self.x - rhs.x,
            y: self.y - rhs.y
        }
    }
}

// Component-wise multiplication
impl <T> Mul for Vec2<T>
    where T: Numeric + Copy + Display {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        Self {
            x: self.x * rhs.x,
            y: self.y * rhs.y
        }
    }
}

// Component-wise division
impl <T> Div for Vec2<T>
    where T: Numeric + Copy + Display {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        Self {
            x: self.x / rhs.x,
            y: self.y / rhs.y
        }
    }
}

impl <T> AddAssign for Vec2<T>
    where T: Numeric + Copy + Display {

    fn add_assign(&mut self, rhs: Self) {
        self.x += rhs.x;
        self.y += rhs.y;
    }
}

impl <T> SubAssign for Vec2<T>
    where T: Numeric + Copy + Display {

    fn sub_assign(&mut self, rhs: Self) {
        self.x -= rhs.x;
        self.y -= rhs.y;
    }
}

// Component-wise multiplication
impl <T> MulAssign for Vec2<T>
    where T: Numeric + Copy + Display {

    fn mul_assign(&mut self, rhs: Self) {
        self.x *= rhs.x;
        self.y *= rhs.y;
    }
}

// Component-wise division
impl <T> DivAssign for Vec2<T>
    where T: Numeric + Copy + Display {

    fn div_assign(&mut self, rhs: Self) {
        self.x /= rhs.x;
        self.y /= rhs.y;
    }
}

// Scalar multiplication
impl <T> Mul<T> for Vec2<T>
    where T: Numeric + Copy + Display {
    type Output = Vec2<T>;
    fn mul(self, rhs: T) -> Self::Output {
        Self { x: self.x * rhs, y: self.y * rhs }
    }
}

impl <T> Zero for Vec2<T>
    where T: Numeric + Copy + Display {
    type Output = Self;

    fn zero() -> Self::Output {
        Self {
            x: T::zero(),
            y: T::zero()
        }
    }
}

impl<T> Display for Vec2<T>
    where T: Numeric + Copy + Display {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "({},{})", self.x, self.y)
    }
}