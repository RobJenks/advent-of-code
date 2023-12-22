pub trait ZipArray<T> {
    fn zip_array(&self, other: &Self, f: fn(&T, &T) -> T) -> Self;
}

impl<T, const N: usize> ZipArray<T> for [T; N]
    where T: Copy + Default {

    fn zip_array(&self, other: &Self, f: fn(&T, &T) -> T) -> Self {
        let mut res = [T::default(); N];
        for i in 0..N {
            res[i] = f(&self[i], &other[i]);
        }
        res
    }
}
