use crate::common::num::BasicArith;
use std::ops::{Add, Sub, Mul, Div};
use std::fmt::{Display, Formatter};
use itertools::Itertools;

pub type Vec3<T> = VecN<T, 3>;
pub type Vec4<T> = VecN<T, 4>;
pub type Vec5<T> = VecN<T, 5>;
pub type Vec6<T> = VecN<T, 6>;
pub type Vec7<T> = VecN<T, 7>;
pub type Vec8<T> = VecN<T, 8>;

#[derive(Clone, Debug, PartialOrd, PartialEq, Eq, Hash)]
pub struct VecN<T, const N: usize>
    where T: BasicArith + Copy {

    comp: [T; N]
}

impl <T, const N: usize> VecN<T, {N}>
    where T: BasicArith + Copy {

    pub fn new(v: [T; N]) -> Self {
        Self { comp: v }
    }

    pub fn scalar_mul(self, scalar: T) -> Self {
        self * Self::new([scalar; N])
    }

    fn apply_component_wise(self, rhs: Self, f: fn(T, T) -> T) -> Self {
        let mut result = [T::zero(); N];
        self.comp.iter().zip(rhs.comp.iter())
            .zip(result.iter_mut())
            .for_each(|((a, b), x)| *x = f(*a, *b));
        Self::new(result)
    }
}

impl <const N: usize> VecN<i32, {N}> {
    pub fn manhattan_dist(&self, to: &Self) -> i32 {
        self.comp.iter().zip(to.comp.iter())
            .fold(0, |acc, (a, b)| acc + (a - b).abs())
    }
}

impl <T, const N: usize> Add for VecN<T, {N}>
    where T: BasicArith + Copy {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        self.apply_component_wise(rhs, Add::add)
    }
}

impl <T, const N: usize> Sub for VecN<T, {N}>
    where T: BasicArith + Copy {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        self.apply_component_wise(rhs, Sub::sub)
    }
}

impl <T, const N: usize> Mul for VecN<T, {N}>
    where T: BasicArith + Copy {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        self.apply_component_wise(rhs, Mul::mul)
    }
}

impl <T, const N: usize> Div for VecN<T, {N}>
    where T: BasicArith + Copy {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        self.apply_component_wise(rhs, Div::div)
    }
}

impl <T, const N: usize> Display for VecN<T, {N}>
    where T: BasicArith + Copy + std::fmt::Display {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}]", self.comp.iter()
            .map(|x| format!("{}", x))
            .intersperse(",".to_string())
            .collect::<String>())
    }
}

#[cfg(test)]
mod tests {
    use crate::common::nvec::{Vec3, Vec5};
    use std::ops::{Add, Sub, Mul, Div};

    #[test]
    fn test_basic_operations() {
        let a = Vec3::<f32>::new([1.,2.,3.]);
        let b = Vec3::<f32>::new([4.,5.,6.]);

        assert_eq!(Vec3::new([5.,7.,9.]), a.clone().add(b.clone()));
        assert_eq!(Vec3::new([-3.,-3.,-3.]), a.clone().sub(b.clone()));
        assert_eq!(Vec3::new([4.,10.,18.]), a.clone().mul(b.clone()));
        assert_eq!(Vec3::new([0.25,0.4,0.5]), a.clone().div(b.clone()));
        assert_eq!(Vec3::new([12.,24.,36.]),a.clone().scalar_mul(12.));
    }

    #[test]
    fn test_display() {
        assert_eq!("[2,4,6,8,10]", format!("{}", Vec5::<i32>::new([2,4,6,8,10])));
    }
}