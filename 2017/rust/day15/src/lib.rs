const INPUT : [usize; 2] = [722, 354];
const FACTOR : [usize; 2] = [16807, 48271];

pub fn run() {
    println!("Part 1 result: {}", part1(INPUT, FACTOR));
    println!("Part 2 result: {}", part2(INPUT, FACTOR));
}

fn part1(input: [usize; 2], factors: [usize; 2]) -> usize {
    let mut gen = input.iter().zip(factors.iter())
        .map(|(x, f)| Generator::new(*x, *f))
        .collect::<Vec<Generator>>();

    (0..40_000_000)
        .map(|_| (gen[0].next().unwrap(), gen[1].next().unwrap()))
        .filter(|(x0, x1)| (x0 & 0xFFFF) == (x1 & 0xFFFF))
        .count()
}

fn part2(input: [usize; 2], factors: [usize; 2]) -> usize {
    let mut gen = input.iter().zip(factors.iter())
        .map(|(x, f)| Generator::new(*x, *f))
        .collect::<Vec<Generator>>();

    (0..5_000_000)
        .map(|_| (gen[0].next_multiple(4).unwrap(), gen[1].next_multiple(8).unwrap()))
        .filter(|(x0, x1)| (x0 & 0xFFFF) == (x1 & 0xFFFF))
        .count()
}


struct Generator {
    factor: usize,
    last: usize
}

impl Generator {
    fn new(x0: usize, factor: usize) -> Self { Self { factor, last: x0 }}

    fn next_multiple(&mut self, mult: usize) -> Option<<Generator as Iterator>::Item> {
        while self.next().unwrap() % mult != 0 { }
        Some(self.last)
    }
}

impl Iterator for Generator {
    type Item = usize;
    fn next(&mut self) -> Option<Self::Item> {
        self.last = (self.last * self.factor) % 2147483647;
        Some(self.last)
    }
}

#[cfg(test)]
mod tests {

    #[test]
    fn test_bit_check() {
        assert_eq!(vec![(1092455, 430625591), (1181022009, 1233683848), (245556042, 1431495498),
            (1744312007, 137874439), (1352636452, 285222916)].iter()

            .filter(|(x,y)| (x & 0xFFFF) == (y & 0xFFFF))
            .count(), 1);
    }
}