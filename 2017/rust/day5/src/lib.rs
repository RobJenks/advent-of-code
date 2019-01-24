extern crate num;

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> usize {
    JumpVec::new(parsed_input(&common::read_file("day5/input.txt")), |_| 1)
        .count() + 1
}

fn part2() -> usize {
    JumpVec::new(parsed_input(&common::read_file("day5/input.txt")), |x| if x >= 3 { -1 } else { 1 })
        .count() + 1
}


struct JumpVec<T> {
    vec : Vec<T>,
    index : i32,
    advance: fn(T) -> T
}

impl <T> JumpVec<T> {
    fn new(data: Vec<T>, advance_fn: fn(T) -> T) -> JumpVec<T> { JumpVec::<T> { vec: data, index: 0, advance: advance_fn } }
}

impl <T> Iterator for JumpVec<T>
    where T: std::marker::Copy,
          T: std::ops::AddAssign,
          T: num::NumCast {
    type Item = T;

    fn next(&mut self) -> Option<T> {
        let jump = self.vec[self.index as usize];
        self.vec[self.index as usize] += (self.advance)(jump);

        self.index += num::cast::<T, i32>(jump).unwrap();
        if self.index >= 0 && self.index < self.vec.len() as i32 { Some(self.vec[self.index as usize]) } else { None }
    }
}

fn parsed_input(input: &String) -> Vec<i32> {
    input.split("\n")
        .map(|x| x.parse::<i32>().unwrap())
        .collect::<Vec<i32>>()
}



#[cfg(test)]
mod tests {
    use super::{JumpVec};

    #[test]
    fn test_jumps() {
        let jump_vec = JumpVec::new(vec![0, 3, 0, 1, -3], |_| 1);

        let result = jump_vec.count() + 1;
        assert_eq!(result, 5);
    }


    #[test]
    fn test_conditional_jumps() {
        let jump_vec = JumpVec::new(vec![0, 3, 0, 1, -3],
                                        |x| if x >= 3 { -1 } else { 1 });

        let result = jump_vec.count() + 1;
        assert_eq!(result, 10);
    }
}