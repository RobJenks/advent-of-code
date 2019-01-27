pub fn run() {
    println!("Part 1 result: {}", part1());
}

fn part1() -> i32 {
    calc_trip(&mut parse_input(common::read_file("day13/input.txt")), 0)
}


fn parse_input(input: String) -> Vec<Layer> {
    let mut layers : Vec<Layer> = vec![];

    input.split("\n")
        .map(|x| x.split(":")
            .map(|s| s.trim().parse::<i32>().unwrap())
            .collect::<Vec<i32>>())
        .for_each(|x| {

            // Fill any empty spaces between layers
            for i in layers.len()..x[0] as usize { layers.push(Layer::new(i as i32, std::i32::MAX, false)); }

            // Add layer
            layers.push(Layer::new(x[0], x[1], true));
        });

    layers
}

fn calc_trip(layers: &mut Vec<Layer>, packet_depth: i32) -> i32 {
    let mut severity = 0i32;
    let n = layers.len();

    for i in 0..n {
        severity += match layers[i].scanner {
            Some(x) if x == packet_depth => layers[i].depth * layers[i].range,
            _ => 0
        };

        layers.iter_mut().for_each(|x| x.step());
    }

    severity
}


#[derive(Debug)]
struct Layer {
    depth: i32,
    range: i32,
    scanner: Option<i32>, // Scanner pos, or None
    scan_dir: i32,        // {-1, +1}
}

impl Layer {
    fn new(depth: i32, range: i32, has_scanner: bool) -> Self {
        Self { depth, range, scanner: if has_scanner { Some(0) } else { None }, scan_dir: 1 }
    }

    fn step(&mut self) {
        if self.scanner.is_none() { return; }

        if (self.scanner.unwrap() + self.scan_dir) < 0 ||
           (self.scanner.unwrap() + self.scan_dir) >= self.range { self.scan_dir *= -1 }

        self.scanner = Some(self.scanner.unwrap() + self.scan_dir);
    }
}



#[cfg(test)]
mod tests {
    use super::{parse_input, calc_trip};

    #[test]
    fn test_severity_calc() {
        assert_eq!(calc_trip(&mut parse_input("0: 3\n1: 2\n4: 4\n6: 4".to_string()), 0), 24);
    }
}