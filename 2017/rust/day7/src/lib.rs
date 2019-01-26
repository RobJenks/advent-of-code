use std::collections::HashMap;

pub fn run() {
    println!("Part 1 result: {}", part1());
}

fn part1() -> String {
    let tree = construct_tree(common::read_file("day7/input.txt"));
    tree.nodes[tree.get_root()].name.clone()
}


struct Tree {
    nodes: Vec<Node>,
    names: HashMap<String, usize>
}

struct Node {
    id: usize,
    name: String,
    weight: u32,
    parent: Option<usize>,
    children: Vec<usize>
}

impl Tree {
    fn new() -> Tree { Tree { nodes: vec![], names: HashMap::new() }}

    fn new_node(&mut self, name: &String) -> usize {
        let id = self.nodes.len();
        self.nodes.push(Node::new(id, name.clone()));
        self.names.insert(name.clone(), id);

        id
    }

    fn get(&self, name: &str) -> Option<usize> {
        match self.names.get(name) {
            Some(id) => Some(*id),
            None => None
        }
    }

    fn get_node(&mut self, name: &str) -> Option<&mut Node> {
        match self.get(name) {
            Some(id) => Some(&mut self.nodes[id]),
            None => None
        }
    }

    fn get_direct(&mut self, name: &str) -> &mut Node {
        match self.get(name) {
            Some(id) => &mut self.nodes[id],
            None => panic!("Node does not exist")
        }
    }

    fn get_or_create(&mut self, name: &str) -> &mut Node{
        match self.get(name) {
            Some(id) => &mut self.nodes[id],
            None => {
                let id = self.new_node(&name.to_string());
                &mut self.nodes[id]
            }
        }
    }

    fn get_root(&self) -> usize {
        let roots = self.nodes.iter()
            .filter(|x| x.parent.is_none())
            .map(|x| x.id)
            .collect::<Vec<usize>>();

        if roots.len() != 1 { panic!("Invalid root count"); }
        roots[0]
    }
}

impl Node {
    fn new(node_id: usize, node_name: String) -> Node {
        Node { id: node_id, name: node_name,  weight: 0, parent: None, children: vec![] }
    }
}

fn construct_tree(input: String) -> Tree {
    let mut tr = Tree::new();

    input.split("\n")
        .map(|x| x.split(&[' ', ','][..]).collect::<Vec<&str>>())
        .for_each(|vec| {
            let node = tr.get_or_create(vec[0]).id;
            tr.nodes[node].weight = vec[1][1..vec[1].len()-1].parse::<u32>().unwrap();

            if vec.len() > 3 {  // Have children
                for i in 3..vec.len() as usize {
                    if !vec[i].is_empty() {
                        let child = tr.get_or_create(vec[i]).id;
                        tr.nodes[child].parent = Some(node);
                        tr.nodes[node].children.push(child);
                    }
                }
            }
        });

    tr
}