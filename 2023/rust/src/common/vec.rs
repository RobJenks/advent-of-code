use std::fmt::{Display, Formatter};
use std::hash::Hash;
use std::ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Sub, SubAssign};
use itertools::Itertools;
use crate::common::array::ZipArray;
use crate::common::num::{ConvFrom, Numeric, Zero};

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Debug, Hash)]
pub struct Vec<T, const N: usize>
    where T: Numeric + Copy + Display + Hash + Default {

    pub data: [T; N],
}

pub type Vec2<T> = Vec<T, 2>;
impl<T> Vec2<T>
    where T: Numeric + Copy + Display + Hash + Default {

    pub fn x(&self) -> T { self.data[0] }
    pub fn y(&self) -> T { self.data[1] }

    pub fn new(x: T, y: T) -> Self {
        Self { data: [x, y] }
    }
}

pub type Vec3<T> = Vec<T, 3>;
impl<T> Vec3<T>
    where T: Numeric + Copy + Display + Hash + Default {

    pub fn x(&self) -> T { self.data[0] }
    pub fn y(&self) -> T { self.data[1] }
    pub fn z(&self) -> T { self.data[2] }

    pub fn new(x: T, y: T, z: T) -> Self {
        Self { data: [x, y, z] }
    }
}

impl <T, const N: usize> Vec<T, N>
    where T: Numeric + Copy + Display + Hash + Default {

    #[allow(unused)]
    pub fn new_with_data(data: [T; N]) -> Self { Self { data } }
    pub fn new_uniform(v: T) -> Self { Self { data: [v; N] } }

    pub fn component_min(&self, other: &Vec<T, N>) -> Self {
        Self { data: self.data.zip_array(&other.data, |a, b| *a.min(b)) }
    }

    pub fn component_max(&self, other: &Vec<T, N>) -> Self {
        Self { data: self.data.zip_array(&other.data, |a, b| *a.max(b)) }
    }

    pub fn product(&self) -> T {
        self.data.iter().cloned().reduce(|prod, x| prod.mul(x)).unwrap()
    }

    pub fn to_dimension<const M: usize>(&self) -> Vec<T, M> {
        let mut data = [T::zero(); M];
        let max = N.min(M);
        for i in 0..max {
            data[i] = self.data[i];
        }

        Vec::<T, M> { data }
    }

    pub fn as_type<U>(&self) -> Vec<U, N>
        where U: ConvFrom<T> + Numeric + Copy + Display + Hash + Default {

        Vec::<U, N> { data: self.data.map(U::convert_from) }
    }
}

impl <T, const N: usize> Numeric for Vec<T, N>
    where T: Numeric + Copy + Display + Hash + Default {
}

impl <T, const N: usize> Add for Vec<T, N>
    where T: Numeric + Copy + Display + Hash + Default {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self { data: self.data.zip_array(&rhs.data, |a, b| *a + *b) }
    }
}

impl <T, const N: usize> Sub for Vec<T, N>
    where T: Numeric + Copy + Display + Hash + Default {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self { data: self.data.zip_array(&rhs.data, |a, b| *a - *b) }
    }
}

// Component-wise multiplication
impl <T, const N: usize> Mul for Vec<T, N>
    where T: Numeric + Copy + Display + Hash + Default {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        Self { data: self.data.zip_array(&rhs.data, |a, b| *a * *b) }
    }
}

// Component-wise division
impl <T, const N: usize> Div for Vec<T, N>
    where T: Numeric + Copy + Display + Hash + Default {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        Self { data: self.data.zip_array(&rhs.data, |a, b| *a / *b) }
    }
}

impl <T, const N: usize> AddAssign for Vec<T, N>
    where T: Numeric + Copy + Display + Hash + Default {

    fn add_assign(&mut self, rhs: Self) {
        for i in 0..N {
            self.data[i] += rhs.data[i];
        }
    }
}

impl <T, const N: usize> SubAssign for Vec<T, N>
    where T: Numeric + Copy + Display + Hash + Default {

    fn sub_assign(&mut self, rhs: Self) {
        for i in 0..N {
            self.data[i] -= rhs.data[i];
        }
    }
}

// Component-wise multiplication
impl <T, const N: usize> MulAssign for Vec<T, N>
    where T: Numeric + Copy + Display + Hash + Default {

    fn mul_assign(&mut self, rhs: Self) {
        for i in 0..N {
            self.data[i] *= rhs.data[i];
        }
    }
}

// Component-wise division
impl <T, const N: usize> DivAssign for Vec<T, N>
    where T: Numeric + Copy + Display + Hash + Default {

    fn div_assign(&mut self, rhs: Self) {
        for i in 0..N {
            self.data[i] /= rhs.data[i];
        }
    }
}

// Scalar multiplication
impl <T, const N: usize> Mul<T> for Vec<T, N>
    where T: Numeric + Copy + Display + Hash + Default {
    type Output = Vec<T, N>;
    fn mul(self, rhs: T) -> Self::Output {
        Self { data: self.data.map(|v| v * rhs) }
    }
}

impl <T, const N: usize> Zero for Vec<T, N>
    where T: Numeric + Copy + Display + Hash + Default {
    type Output = Self;

    fn zero() -> Self::Output {
        Self { data: [T::zero(); N] }
    }
}

impl<T, const N: usize> Display for Vec<T, N>
    where T: Numeric + Copy + Display + Hash + Default {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "({})", self.data.iter().join(","))
    }
}