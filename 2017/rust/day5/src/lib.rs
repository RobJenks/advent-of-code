extern crate num;

pub fn run() {
    println!("Part 1 result: {}", part1());
}

fn part1() -> u32 {
    let mut jump_vec = JumpVec::new(parsed_input(&common::read_file("day5/input.txt")));

    let mut result : u32 = 1;
    while jump_vec.next() != None { result += 1 }

    result
}


struct JumpVec<T> {
    vec : Vec<T>,
    index : i32
}

impl <T> JumpVec<T> {
    fn new(data: Vec<T>) -> JumpVec<T> { JumpVec::<T> { vec: data, index: 0 } }
}

impl <T> Iterator for JumpVec<T>
    where T: std::marker::Copy,
          T: std::ops::AddAssign,
          T: num::NumCast {
    type Item = T;

    fn next(&mut self) -> Option<T> {
        let jump = self.vec[self.index as usize];
        self.vec[self.index as usize] += num::cast::<i32, T>(1).unwrap();

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
        let mut jump_vec = JumpVec::new(vec![0, 3, 0, 1, -3]);

        let mut result : u32 = 1;
        while jump_vec.next() != None { result += 1 }

        assert_eq!(result, 5);
    }
}