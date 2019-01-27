
pub struct Hash {
    data: Vec<u32>,     // Ring buffer
    ptr: isize,         // Current position pointer
    skip: usize,        // Current skip length
    repeat: bool,       // Cycle the iterator indefinitely, if set

    input: Vec<usize>,  // Vector of input lengths
    in_ptr: usize       // Pointer to current input element
}

impl Hash {
    pub fn new(size: u32, input: Vec<usize>, cycle: bool) -> Self { Self { data: (0..size).collect(), ptr: 0, skip: 0, input, in_ptr: 0, repeat: cycle }}
}

impl Iterator for Hash {
    type Item = Vec<u32>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.in_ptr == self.input.len() { self.in_ptr = 0; if !self.repeat { return None; }}

        let length = self.input[self.in_ptr] as isize;
        self.in_ptr += 1;

        for i in 0..(length/2) as isize {
            let indices = (((self.ptr + i) % self.data.len() as isize) as usize,
                           (((self.ptr + (length-1))-i) % self.data.len() as isize) as usize);
            self.data.swap(indices.0, indices.1);
        }

        self.ptr = (self.ptr + length + self.skip as isize) % self.data.len() as isize;
        self.skip += 1;

        Some(self.data.clone())
    }
}

pub fn calculate_ascii_hash_hex(input_data: String) -> String {
    calculate_dense_hash(input_data).iter().map(|x| format!("{:01$x}", x, 2)).collect::<String>()
}

pub fn calculate_ascii_hash_binary(input_data: String) -> String {
    calculate_dense_hash(input_data).iter().map(|x| format!("{0:01$b}", x, 8)).collect::<String>()
}

fn calculate_dense_hash(input_data: String) -> Vec<u32> {
    let mut input = input_data.into_bytes().iter().map(|x| *x as usize).collect::<Vec<usize>>();
    input.append(&mut vec![17, 31, 73, 47, 23]);

    let cycle_length = input.len();
    let data = Hash::new(256, input, true)
        .take(cycle_length * 64)
        .last().unwrap();

    let mut dense = vec![0u32; 16];
    let mut iter = data.iter();
    for i in 0..16 {
        dense[i] = iter.by_ref().take(16).fold(0, |acc,x| if acc == 0 { *x } else { acc ^ *x });
    }

    dense
}




#[cfg(test)]
mod tests {
    use super::{Hash, calculate_ascii_hash_hex};

    #[test]
    fn test_hash() {
        assert_eq!(Hash::new(5, vec![3, 4, 1, 5], false).last().unwrap(), vec![3, 4, 2, 1, 0]);
    }

    #[test]
    fn test_ascii_hex_hash() {
        assert_eq!(calculate_ascii_hash_hex("".to_string()), "a2582a3a0e66e6e86e3812dcb672a272");
        assert_eq!(calculate_ascii_hash_hex("AoC 2017".to_string()), "33efeb34ea91902bb2f59c9920caa6cd");
        assert_eq!(calculate_ascii_hash_hex("1,2,3".to_string()), "3efbe78a8d82f29979031a4aa0b16a9d");
        assert_eq!(calculate_ascii_hash_hex("1,2,4".to_string()), "63960835bcdc130f0b66d7ff4f6a5a8e");
    }

    #[test]
    fn test_ascii_binary_hash() {
        assert_eq!(format!("{0:01$b}", 17, 8), "00010001");
        assert_eq!(format!("{0:01$b}", 168566807, 8), "1010000011000010000000010111");
    }
}