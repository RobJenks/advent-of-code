pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> i32 {
    calc_trip(&mut parse_input(common::read_file("day13/input.txt")), true)
}

fn part2() -> i32 {
    calc_minimum_delay(&parse_input(common::read_file("day13/input.txt")))
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

fn calc_trip(layers: &mut Vec<Layer>, permit_catch: bool) -> i32 {
    const PACKET_DEPTH : i32 = 0;
    let mut severity = 0i32;
    let n = layers.len();

    for i in 0..n {
        severity += match layers[i].scanner {
            Some(x) if x == PACKET_DEPTH && !permit_catch => return std::i32::MAX,
            Some(x) if x == PACKET_DEPTH => layers[i].depth * layers[i].range,
            _ => 0
        };

        timestep(layers);
    }

    severity
}

fn calc_minimum_delay(initial_state: &Vec<Layer>) -> i32 {
    let mut layers = initial_state.clone();
    (1..).take_while(|_| calc_trip(&mut timestep(&mut layers).clone(), false) != 0).max().unwrap() + 1
}

fn timestep(layers: &mut Vec<Layer>) -> &mut Vec<Layer> {
    layers.iter_mut().for_each(|x| x.step());
    layers
}


#[derive(Debug, Clone)]
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
    use super::{parse_input, calc_trip, calc_minimum_delay};

    #[test]
    fn test_severity_calc() {
        assert_eq!(calc_trip(&mut parse_input("0: 3\n1: 2\n4: 4\n6: 4".to_string()), true), 24);
    }

    #[test]
    fn test_delay() {
        assert_eq!(calc_minimum_delay(&parse_input("0: 3\n1: 2\n4: 4\n6: 4".to_string())), 10);
    }
}