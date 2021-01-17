use crate::common::num::BasicArith;
use std::ops::{Add, Sub, Mul, Div};

#[derive(Clone, Copy, Debug, PartialOrd, PartialEq)]
pub struct Vec2<T>
    where T: BasicArith + Copy {

    pub x: T,
    pub y: T
}

impl <T> Vec2<T>
    where T: BasicArith + Copy {

    pub fn new(x: T, y: T) -> Self {
        Self { x, y }
    }
}

impl <T> Add for Vec2<T>
    where T: BasicArith + Copy {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self { x: self.x + rhs.x, y: self.y + rhs.y }
    }
}

impl <T> Sub for Vec2<T>
    where T: BasicArith + Copy {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self { x: self.x - rhs.x, y: self.y - rhs.y }
    }
}

impl <T> Mul for Vec2<T>
    where T: BasicArith + Copy {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        Self { x: self.x * rhs.x, y: self.y * rhs.y }
    }
}

impl <T> Div for Vec2<T>
    where T: BasicArith + Copy {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        Self { x: self.x / rhs.x, y: self.y / rhs.y }
    }
}