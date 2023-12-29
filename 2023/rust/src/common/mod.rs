pub mod num;
pub mod vec;
pub mod grid;
pub mod grid3d;
pub mod math;
pub mod array;
pub mod pathfinding;
pub mod graph;
mod util;

pub fn read_file(path : &str) -> String {
    match std::fs::read_to_string(path) {
        Err(e) => panic!("Failed to read \"{}\": {}", path, e),
        Ok(res) => res
    }
}
