const INPUT : usize = 343;

pub fn run() {
    println!("Part 1 result: {}", part1(INPUT));
}

fn part1(input: usize) -> usize {
    let mut sl = SpinLock::new(input);
    let ptr = sl.insert_multiple(2017);

    sl.data[(ptr + 1) % sl.data.len()]
}


pub struct SpinLock {
    data: Vec<usize>,
    ptr: usize,
    step: usize
}

impl SpinLock {
    pub fn new(step: usize) -> Self { Self { data: vec![0], ptr: 0, step }}

    pub fn insert(&mut self) -> usize {
        self.ptr = (self.ptr + self.step + 1) % self.data.len();

        let val = self.data.len();
        self.data.insert(self.ptr, val);

        self.ptr
    }

    pub fn insert_multiple(&mut self, n: usize) -> usize { (0..n).map(|_| self.insert()).last().unwrap() }
}

#[cfg(test)]
mod tests {
    use super::{SpinLock};

    #[test]
    fn test_insertion() {
        let mut sl = SpinLock::new(3);
        let n = 9usize;
        sl.insert_multiple(n);

        assert_eq!(sl.ptr, sl.data.iter().position(|x| *x == 9).unwrap());
        assert_eq!((0..n)
            .map(|_| { sl.data.rotate_left(1); sl.data.clone() })
            .any(|vec| vec == vec![0,9,5,7,2,4,3,8,6,1]),
        true);
    }

}