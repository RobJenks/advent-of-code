pub fn run() {
    println!("Part 1 result: {}", part1());
}

fn part1() -> u32 {
    Stream::new(common::read_file("day9/input.txt").as_str()).map(|(_, x)| x).sum()
}

struct Stream {
    data: Vec<char>,
    ptr: usize,
    level: u32,
}

impl Stream {
    fn new(input: &str) -> Self { Stream { data: input.chars().collect::<Vec<char>>(), ptr: 0, level: 0 } }
}

impl Iterator for Stream {
    type Item = (usize, u32);       // { index, group level }

    fn next(&mut self) -> Option<Self::Item> {
        let mut in_garbage = false;
        loop {
            match self.data[self.ptr] {
                '{' if !in_garbage => {
                    self.level += 1;
                    self.ptr += 1;
                    return Some((self.ptr - 1, self.level));
                }
                '}' if !in_garbage => {
                    self.level -= 1;
                    if self.level == 0 { return None; }
                }
                '<' => { in_garbage = true; }
                '>' => { in_garbage = false; }
                '!' => { self.ptr += 1; }      // Skip next char
                _ => ()
            }

            self.ptr += 1;
        }
    }
}


#[cfg(test)]
mod tests {
    use super::Stream;

    #[test]
    fn test_count() {
        assert_eq!(Stream::new("{}").count(), 1);
        assert_eq!(Stream::new("{{{}}}").count(), 3);
        assert_eq!(Stream::new("{{},{}}").count(), 3);
        assert_eq!(Stream::new("{{{},{},{{}}}}").count(), 6);
        assert_eq!(Stream::new("{<{},{},{{}}>}").count(), 1);
        assert_eq!(Stream::new("{<a>,<a>,<a>,<a>}").count(), 1);
        assert_eq!(Stream::new("{{<a>},{<a>},{<a>},{<a>}}").count(), 5);
        assert_eq!(Stream::new("{{<!>},{<!>},{<!>},{<a>}}").count(), 2);
    }

    #[test]
    fn test_scores() {
        assert_eq!(Stream::new("{}").map(|(_, x)| x).sum::<u32>(), 1);
        assert_eq!(Stream::new("{{{}}}").map(|(_, x)| x).sum::<u32>(), 6);
        assert_eq!(Stream::new("{{},{}}").map(|(_, x)| x).sum::<u32>(), 5);
        assert_eq!(Stream::new("{{{},{},{{}}}}").map(|(_, x)| x).sum::<u32>(), 16);
        assert_eq!(Stream::new("{<a>,<a>,<a>,<a>}").map(|(_, x)| x).sum::<u32>(), 1);
        assert_eq!(Stream::new("{{<ab>},{<ab>},{<ab>},{<ab>}}").map(|(_, x)| x).sum::<u32>(), 9);
        assert_eq!(Stream::new("{{<!!>},{<!!>},{<!!>},{<!!>}}").map(|(_, x)| x).sum::<u32>(), 9);
        assert_eq!(Stream::new("{{<a!>},{<a!>},{<a!>},{<ab>}}").map(|(_, x)| x).sum::<u32>(), 3);
    }
}