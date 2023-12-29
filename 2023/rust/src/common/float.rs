use std::cmp::Ordering;
use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};
use std::num::ParseFloatError;
use std::ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Sub, SubAssign};
use std::str::FromStr;
use ordered_float::{FloatCore, OrderedFloat};
use crate::common;
use crate::common::num::{Numeric, Zero};

// Float type with total ordering, to allow e.g. Eq & Cmp
#[derive(Copy, Clone, Debug)]
pub struct StandardFloat<T>(OrderedFloat<T>);

pub type F32 = StandardFloat<f32>;
pub type F64 = StandardFloat<f64>;


impl<T> Add for StandardFloat<T>
    where T: FloatCore + Display + Zero<Output=T> {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self(self.0.add(rhs.0))
    }
}

impl<T> Sub for StandardFloat<T>
    where T: FloatCore + Display + Zero<Output=T> {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self(self.0.sub(rhs.0))
    }
}

impl<T> Mul for StandardFloat<T>
    where T: FloatCore + Display + Zero<Output=T> {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        Self(self.0.mul(rhs.0))
    }
}

impl<T> Div for StandardFloat<T>
    where T: FloatCore + Display + Zero<Output=T> {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        Self(self.0.div(rhs.0))
    }
}

impl<T> AddAssign for StandardFloat<T>
    where T: FloatCore + Display + Zero<Output=T> {
    fn add_assign(&mut self, rhs: Self) {
        self.0 = self.0.add(rhs.0)
    }
}

impl<T> SubAssign for StandardFloat<T>
    where T: FloatCore + Display + Zero<Output=T> {
    fn sub_assign(&mut self, rhs: Self) {
        self.0 = self.0.sub(rhs.0)
    }
}

impl<T> MulAssign for StandardFloat<T>
    where T: FloatCore + Display + Zero<Output=T> {
    fn mul_assign(&mut self, rhs: Self) {
        self.0 = self.0.mul(rhs.0)
    }
}

impl<T> DivAssign for StandardFloat<T>
    where T: FloatCore + Display + Zero<Output=T> {
    fn div_assign(&mut self, rhs: Self) {
        self.0 = self.0.div(rhs.0)
    }
}

impl<T> Ord for StandardFloat<T>
    where T: FloatCore + Display + Zero<Output=T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0.cmp(&other.0)
    }
}

impl<T> Eq for StandardFloat<T>
    where T: FloatCore + Display + Zero<Output=T> {}

impl<T> PartialEq<Self> for StandardFloat<T>
    where T: FloatCore + Display + Zero<Output=T> {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(&other.0)
    }
}

impl<T> PartialOrd<Self> for StandardFloat<T>
    where T: FloatCore + Display + Zero<Output=T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.0.partial_cmp(&other.0)
    }
}

impl<T> Numeric for StandardFloat<T>
    where T: FloatCore + Display + Zero<Output=T> { }

impl<T> Zero for StandardFloat<T>
    where T: FloatCore + Display + Zero<Output=T> {
    type Output = Self;
    fn zero() -> Self::Output { StandardFloat(OrderedFloat(<T as common::num::Zero>::zero())) }
}

impl<T> Display for StandardFloat<T>
    where T: FloatCore + Display + Zero<Output=T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.0)
    }
}

impl<T> Default for StandardFloat<T>
    where T: FloatCore + Display + Zero<Output=T> {
    fn default() -> Self {
        Self::zero()
    }
}

impl<T> Hash for StandardFloat<T>
    where T: FloatCore + Display + Zero<Output=T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl From<f32> for F32 {
    fn from(value: f32) -> Self {
        Self(OrderedFloat(value))
    }
}

impl From<f64> for F64 {
    fn from(value: f64) -> Self {
        Self(OrderedFloat(value))
    }
}

impl FromStr for F32 {
    type Err = ParseFloatError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        f32::from_str(s).map(|x| StandardFloat::<f32>(OrderedFloat(x)))
    }
}

impl FromStr for F64 {
    type Err = ParseFloatError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        f64::from_str(s).map(|x| StandardFloat::<f64>(OrderedFloat(x)))
    }
}

impl<T> PartialEq<T> for StandardFloat<T>
    where T: FloatCore + Display + Zero<Output=T> {
    fn eq(&self, other: &T) -> bool {
        self.0.0.eq(&other)
    }
}

impl<T> PartialOrd<T> for StandardFloat<T>
    where T: FloatCore + Display + Zero<Output=T> {
    fn partial_cmp(&self, other: &T) -> Option<Ordering> {
        self.0.0.partial_cmp(&other)
    }
}