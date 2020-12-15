pub fn read_file(path : &str) -> String {
    match std::fs::read_to_string(path) {
        Err(e) => panic!("Failed to read \"{}\": {}", path, e),
        Ok(res) => res
    }
}
