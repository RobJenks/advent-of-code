use std::ops::{Add, Mul, Sub, Div};
use num_traits::identities::Zero;

pub trait BasicArith
    : Add<Output = Self>
    + Sub<Output = Self>
    + Mul<Output = Self>
    + Div<Output = Self>
    + Zero<Output = Self>
    where Self: std::marker::Sized { }

impl <T> BasicArith for T
    where T: Add<Output=T> + Sub<Output=T> + Mul<Output=T> + Div<Output=T> + Zero<Output=T> { }
