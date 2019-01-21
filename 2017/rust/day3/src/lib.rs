use common::Vec2;

pub fn run() {
    const INPUT: u32 = 361527;

    println!("Part 1 result: {}", part1(INPUT));
}

fn part1(input: u32) -> i32 {
    get_coord(input).manhattan(Vec2::<i32>{x: 0, y: 0})
}

fn get_coord(val : u32) -> Vec2<i32> {
    // Each concentric ring of cells will increase in count by 8 at each level
    // Total cells therefore increases by (last_size += 8) at each level
    // Values for a ring always begin at the bottom-right cell of the ring
    // We can therefore quickly determine the ring that the target value
    // exists in, and then which cell of that ring specifically based on its value

    // Determine the level that this value belongs in
    let level = SpiralIter::new().enumerate().take_while(|(_, x)| x < &val).last().unwrap();

    // We now know that the value exists in level [0]+1+1 (zero based, and we have the index of the last ring
    // before the one that it exists in), with [1] the value in the last cell of the previous ring
    let prior_ring_number = (level.0 + 1) as i32;
    let mut current_value = level.1 + 1;
    let mut pos = Vec2::<i32> { x: prior_ring_number + 1, y: prior_ring_number };

    // Iterate around this ring until we reach the target value
    let sl = (prior_ring_number + 1) * 2;

    for (x_inc, y_inc, side) in &[(0,-1,sl-1), (-1,0,sl), (0,1,sl), (1,0,sl+1)] {
        for _ in 0..(*side) {
            if current_value == val { return pos; }
            current_value += 1;
            pos.x += x_inc;
            pos.y += y_inc;
        }
    }

    panic!("No result was found...");
}

#[derive(Debug)]
struct SpiralIter {
    level_count : u32,
    total_count : u32
}

impl SpiralIter {
    fn new() -> SpiralIter {
        SpiralIter { level_count: 0, total_count: 0 }
    }
}

impl Iterator for SpiralIter {
    type Item = u32;

    fn next(&mut self) -> Option<u32> {
        self.level_count += 8u32;
        self.total_count += self.level_count;

        // Infinite iterator; no possibility of returning "None"
        Some(self.total_count + 1)   // +1 for centre cell
    }
}




#[cfg(test)]
mod tests {
    use common::Vec2;

    #[test]
    fn test_coords() {
        assert_eq!(crate::get_coord(28), Vec2::<i32>{ x: 3, y: 0 });
        assert_eq!(crate::get_coord(22), Vec2::<i32>{ x: -1, y: 2 });
        assert_eq!(crate::get_coord(80), Vec2::<i32>{ x: 3, y: 4 });
        assert_eq!(crate::get_coord(81), Vec2::<i32>{ x: 4, y: 4 });
        assert_eq!(crate::get_coord(50), Vec2::<i32>{ x: 4, y: 3 });
    }

    #[test]
    fn test_dist() {
        assert_eq!(crate::get_coord(28).manhattan(Vec2::<i32> { x: 0, y: 0 }), 3);
        assert_eq!(crate::get_coord(22).manhattan(Vec2::<i32> { x: 0, y: 0 }), 3);
        assert_eq!(crate::get_coord(80).manhattan(Vec2::<i32> { x: 0, y: 0 }), 7);
        assert_eq!(crate::get_coord(81).manhattan(Vec2::<i32> { x: 0, y: 0 }), 8);
        assert_eq!(crate::get_coord(50).manhattan(Vec2::<i32> { x: 0, y: 0 }), 7);
    }
}
