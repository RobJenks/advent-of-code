use std::ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Sub, SubAssign};
use crate::common::num::{Numeric, Zero};

#[derive(Copy, Clone,Eq, PartialEq, Debug)]
pub struct Vec2<T>
    where T: Numeric + Copy {

    pub x: T,
    pub y: T
}

impl <T> Vec2<T>
    where T: Numeric + Copy {

    pub fn new(x: T, y: T) -> Self { Self { x, y } }
}

impl <T> Numeric for Vec2<T>
    where T: Numeric + Copy {
}

impl <T> Add for Vec2<T>
    where T: Numeric + Copy {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self {
            x: self.x + rhs.x,
            y: self.y + rhs.y
        }
    }
}

impl <T> Sub for Vec2<T>
    where T: Numeric + Copy {
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
    where T: Numeric + Copy {
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
    where T: Numeric + Copy {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        Self {
            x: self.x / rhs.x,
            y: self.y / rhs.y
        }
    }
}

impl <T> AddAssign for Vec2<T>
    where T: Numeric + Copy {

    fn add_assign(&mut self, rhs: Self) {
        self.x += rhs.x;
        self.y += rhs.y;
    }
}

impl <T> SubAssign for Vec2<T>
    where T: Numeric + Copy {

    fn sub_assign(&mut self, rhs: Self) {
        self.x -= rhs.x;
        self.y -= rhs.y;
    }
}

// Component-wise multiplication
impl <T> MulAssign for Vec2<T>
    where T: Numeric + Copy {

    fn mul_assign(&mut self, rhs: Self) {
        self.x *= rhs.x;
        self.y *= rhs.y;
    }
}

// Component-wise division
impl <T> DivAssign for Vec2<T>
    where T: Numeric + Copy {

    fn div_assign(&mut self, rhs: Self) {
        self.x /= rhs.x;
        self.y /= rhs.y;
    }
}

impl <T> Zero for Vec2<T>
    where T: Numeric + Copy {
    type Output = Self;

    fn zero() -> Self::Output {
        Self {
            x: T::zero(),
            y: T::zero()
        }
    }
}
