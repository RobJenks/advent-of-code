
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
        .map(|x| x.chars().next().expect("Invalid input bitstring") == '#')
        .rev()
        .collect::<Vec<_>>();

    let right = lines.iter().skip(1)    // Top to bottom
        .map(|x| x.chars().nth(9).expect("Invalid input bitstring") == '#')
        .collect::<Vec<_>>();

    TileDef::new(id, [edge_value(up), edge_value(right), edge_value(down), edge_value(left)])
}

fn edge_value(comp: Vec<bool>) -> u32 {
    comp.iter().enumerate()
        .fold(0, |acc, (i, x)| acc + if *x { 2u32.pow(i as u32) } else { 0 })
}

// Can do all this bitwise
pub fn flip_edge(edge: u32) -> u32 {
    let bin = (0..10)
        .map(|x| if (edge & (1 << x)) != 0 { 1u32 } else { 0 })
        .collect::<Vec<_>>();

    bin.iter()
        .rev()
        .enumerate()
        .fold(0, |acc, (i, b)| acc + (2u32.pow(i as u32) * b))
}

pub fn determine_layout(tile_defs: Vec<TileDef>) -> TileMap {
    let mut defs = tile_defs.clone();

    // Pick any starting tile def
    let init = defs.pop().expect("No tile defs");
    let mut tiles = vec![Tile::new(init.id, init.edges_normal)];

    // Build tile map incrementally until no remaining tile defs
    while !defs.is_empty() {
        let (def_id, conn) = defs.iter().enumerate()
            .map(|(i, def)| (i, find_connection(def, &tiles)))
            .filter(|(i, def)| def.is_some())
            .map(|(i, def)| (i, def.unwrap()))
            .next()
            .unwrap_or_else(|| panic!("No valid connection at tilemap size = {}, defs size = {}", tiles.len(), defs.len()));

        let id = tiles.len();
        tiles.push((&conn).0.clone());
        tiles.get_mut(conn.1.tile as usize)
            .unwrap_or_else(|| panic!("Invalid connection index {}", conn.1.tile))
            .connect(conn.1.dir, conn.1.tile);

        defs.remove(def_id);
    }

    TileMap::new(tiles)
}

// If a connection between the given definition and current tileset is possible, returns the new tile to be added and the direction/tile where it should be connected
pub fn find_connection(def: &TileDef, tiles: &Vec<Tile>) -> Option<(Tile, TileConn)> {
    let opts = [&def.edges_normal, &def.edges_flip_v, &def.edges_flip_h];
    for tile in tiles {
        for opt in opts {
            if tile.edges[Dir::UP as usize] == opt[Dir::DOWN as usize] { return Some(tile_with_conn(def.id, opt, Dir::DOWN, tile.id)); }
            if tile.edges[Dir::RIGHT as usize] == opt[Dir::LEFT as usize] { return Some(tile_with_conn(def.id, opt, Dir::LEFT, tile.id)); }
            if tile.edges[Dir::DOWN as usize] == opt[Dir::UP as usize] { return Some(tile_with_conn(def.id, opt, Dir::UP, tile.id)); }
            if tile.edges[Dir::LEFT as usize] == opt[Dir::RIGHT as usize] { return Some(tile_with_conn(def.id, opt, Dir::RIGHT, tile.id)); }
        };
    }

    None
}

// Returns a tile with connection to the target tile in the specified direction, plus the required TileConn to be applied to the target tile
fn tile_with_conn(id: u32, edges: &[u32; 4], dir: Dir, connected_tile: u32) -> (Tile, TileConn) {
    let mut tile = Tile::new(id, edges.clone());
    tile.connect(dir, connected_tile);

    (tile, TileConn::new(connected_tile, Dir::opposite(dir)))
}

#[repr(usize)]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Dir {
    UP = 0usize,
    RIGHT = 1,
    DOWN = 2,
    LEFT = 3
}

impl Dir {
    pub fn opposite(dir: Dir) -> Dir {
        match dir {
            Dir::UP     => Dir::DOWN,
            Dir::RIGHT  => Dir::LEFT,
            Dir::DOWN   => Dir::UP,
            _           => Dir::RIGHT
        }
    }
}

impl Into<usize> for Dir {
    fn into(self) -> usize { self as usize }
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub struct Tile {
    id: u32,
    edges: [u32; 4],    // Edge values
    adj: [u32; 4]       // Adjacent tiles; 0 if none
}

impl Tile {
    pub fn new(id: u32, edges: [u32; 4]) -> Self {
        Self { id, edges, adj: [0, 0, 0, 0] }
    }

    pub fn connect(&mut self, direction: Dir, adj: u32) {
        assert!((direction as usize) >= 0 && (direction as usize) < 4);
        self.adj[direction as usize] = adj;
    }

    pub fn edge(&self, dir: Dir) -> u32 { self.edges[dir as usize] }
}

#[derive(Clone, Debug)]
pub struct TileMap {
    tiles: Vec<Tile>
}

impl TileMap {
    pub fn new(tiles: Vec<Tile>) -> Self {
        Self { tiles }
    }

    pub fn get_tiles(&self) -> &Vec<Tile> {
        &self.tiles
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TileConn {
    tile: u32,
    dir: Dir
}

impl TileConn {
    pub fn new(tile: u32, dir: Dir) -> Self { Self { tile, dir }}
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
