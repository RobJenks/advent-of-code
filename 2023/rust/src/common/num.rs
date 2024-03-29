use std::hash::Hash;
use std::ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Sub, SubAssign};

pub trait Zero {
    type Output;
    fn zero() -> Self::Output;
}

pub trait Numeric
    : Add<Output = Self>
    + Sub<Output = Self>
    + Mul<Output = Self>
    + Div<Output = Self>
    + AddAssign
    + SubAssign
    + MulAssign
    + DivAssign
    + Ord + PartialOrd
    + Eq + PartialEq
    + Hash
    + Zero<Output = Self>
    where Self: Sized {

}

impl Numeric for i32 {}
impl Numeric for i64 {}
impl Numeric for isize {}
impl Numeric for u32 {}
impl Numeric for u64 {}
impl Numeric for usize {}

impl Zero for i32 {
    type Output = Self;
    fn zero() -> Self { 0 }
}

impl Zero for i64 {
    type Output = Self;
    fn zero() -> Self::Output { 0 }
}

impl Zero for isize {
    type Output = Self;
    fn zero() -> Self::Output { 0 }
}

impl Zero for u32 {
    type Output = Self;
    fn zero() -> Self::Output { 0 }
}

impl Zero for u64 {
    type Output = Self;
    fn zero() -> Self::Output { 0 }
}

impl Zero for usize {
    type Output = Self;
    fn zero() -> Self::Output { 0 }
}

impl Zero for f32 {
    type Output = Self;
    fn zero() -> Self::Output { 0.0 }
}

impl Zero for f64 {
    type Output = Self;
    fn zero() -> Self::Output { 0.0 }
}

// Trait signalling potential conversion, as long as type bounds & loss of information are acceptable 
pub trait ConvFrom<T> {
    fn convert_from(value: T) -> Self;
}

impl ConvFrom<isize> for usize { fn convert_from(value: isize) -> usize { value as usize } }
impl ConvFrom<usize> for isize { fn convert_from(value: usize) -> isize { value as isize } }
impl ConvFrom<i32> for usize { fn convert_from(value: i32) -> usize { value as usize } }
impl ConvFrom<usize> for i32 { fn convert_from(value: usize) -> i32 { value as i32 } }
