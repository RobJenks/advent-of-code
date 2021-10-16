
pub fn run() {
    println!("Part 1 result: {}", part1());
}

fn part1() -> usize {
    12
}

fn parse_input(data: String) -> Vec<TileDef> {
    data.split("\n\n")
        .map(|s| parse_input_block(s))
        .collect()
}

fn parse_input_block(block: &str) -> TileDef {
    let lines = block.lines().collect::<Vec<_>>();

    let id = lines[0].replace("Tile ", "").replace(":", "")
        .parse::<u32>().unwrap_or_else(|e| panic!("Cannot parse tile ID ({})", e));

    let up = lines[1].chars().map(|c| c == '#').collect::<Vec<_>>();            // L to R
    let down = lines[10].chars().rev().map(|c| c == '#').collect::<Vec<_>>();   // R to L

    let left = lines.iter().skip(1)     // Bottom to Top
        .map(|x| x[0] == '#')
        .rev()
        .collect::<Vec<_>>();

    let right = lines.iter().skip(1)    // Top to bottom
        .map(|x| x[9] == '#')
        .collect::<Vec<_>>();

    TileDef::new(id, [edge_value(up), edge_value(right), edge_value(bottom), edge_value(left)])
}

fn edge_value(comp: Vec<bool>) -> u32 {
    comp.iter().enumerate()
        .fold(0, |acc, (i, x)| acc + if x { 2u32.pow(i as u32) } else { 0 })
}

pub fn flip_edge(edge: u32) -> u32 {
    let bin = (0..10)
        .map(|x| if (edge & (1 << x)) != 0 { 1u32 } else { 0 })
        .collect::<Vec<_>>();

    bin.iter()
        .rev()
        .enumerate()
        .fold(0, |acc, (i, b)| acc + (2u32.pow(i as u32) * b))
}

pub struct TileDef {
    id: u32,
    edges_normal: [u32; 4],
    edges_flip_v: [u32; 4],     // Flipped about vertical (up and down will change)
    edges_flip_h: [u32; 4],     // Flipped about horizontal (left and right will change)
}

impl TileDef {
    pub fn new(id: u32, edges: [u32; 4]) -> Self {
        Self {
            id,
            edges_normal: edges.clone(),
            edges_flip_v: [flip_edge(edges[0]), edges[1], flip_edge(edges[2]), edges[3]],
            edges_flip_h: [edges[0], flip_edge(edges[1]), edges[2], flip_edge(edges[3])]
        }
    }
}

pub struct Tile {
    id: u32,
    edges: [u32; 4],    // Edge values
    adj: [u32; 4]       // Adjacent tiles; 0 if none
}

impl Tile {
    pub fn new(id: u32, edges: [u32; 4]) -> Self {
        Self { id, edges, adj: [0, 0, 0, 0] }
    }

    pub fn connect(&mut self, direction: usize, adj: u32) {
        assert!(direction >= 0 && direction < 4);
        self.adj[direction] = adj;
    }
}


#[cfg(test)]
mod tests {
    use crate::day20::*;

    #[test]
    fn test_edge_flip() {
        assert_eq!(flip_edge(0), 0);            // Empty
        assert_eq!(flip_edge(1023), 1023);      // Full

        assert_eq!(flip_edge(7), 896);          // 0...0111  --> 1110...0
        assert_eq!(flip_edge(896), 7);          // 1110...0  --> 0...0111
    }

}
