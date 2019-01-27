use common::hex::{Hex, HexDir};

pub fn run() {
    println!("Part 1 result: {}", part1());
}

fn part1() -> i32 {
    origin_dist(
        &common::read_file("day11/input.txt")
            .split(",")
            .map(|x| HexDir::from_str(x))
            .collect::<Vec<HexDir>>()
    )

}

fn origin_dist(directions: &Vec<HexDir>) -> i32 {
    Hex::origin().clone().follow_path(directions).dist(&Hex::origin())
}






#[cfg(test)]
mod tests {
    use common::hex::{HexDir};
    use super::{ origin_dist };

    #[test]
    fn test_origin_dist() {
        assert_eq!(origin_dist(&vec![HexDir::NE, HexDir::NE, HexDir::NE]), 3);
        assert_eq!(origin_dist(&vec![HexDir::NE, HexDir::NE, HexDir::SW, HexDir::SW]), 0);
        assert_eq!(origin_dist(&vec![HexDir::NE, HexDir::NE, HexDir::S, HexDir::S]), 2);
        assert_eq!(origin_dist(&vec![HexDir::SE, HexDir::SW, HexDir::SE, HexDir::SW, HexDir::SW]), 3);
    }
}


