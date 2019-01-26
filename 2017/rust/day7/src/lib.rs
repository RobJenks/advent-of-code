use std::collections::HashMap;
use std::collections::VecDeque;

pub fn run() {
    println!("Part 1 result: {}",   part1());
    println!("Part 2 result: {:?}", part2());
}

fn part1() -> String {
    let tree = construct_tree(common::read_file("day7/input.txt"));
    tree.nodes[tree.get_root()].name.clone()
}

fn part2() -> (u32, u32) {
    let tree = construct_tree(common::read_file("day7/input.txt"));
    let wt = cumulative_weights(&tree);

    let unbalanced = tree.nodes.iter()
        .map(|n| n.children.iter()
            .map(|x| wt[*x])
            .fold((n.id, 0, false), |(n,val,diff), x| if x == val || val == 0 { (n, x, diff) } else { (n, val, true) } )
        )
        .filter(|(_, _, diff)| *diff)
        .map(|(n, _, _)| n)
        .collect::<Vec<usize>>();

    // There will be multiple unbalanced nodes; since only one weight can be changed, if all other nodes were balanced
    // then updating the unbalanced node would force its parents out of balance.  Select the lightest node as the one
    // furthest from the root and therefore the one which needs rebalancing
    unbalanced.iter()
        .map(|n| tree.nodes[*n].children.iter()
            .map(|c| wt[*c])
            .fold((std::u32::MAX, std::u32::MIN), |(mn, mx), x| (std::cmp::min(mn, x), std::cmp::max(mx, x)))
        )
        .min_by(|(x0,_),(x1,_)| x0.cmp(x1)).unwrap()
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

fn cumulative_weights(tree: &Tree) -> Vec<u32> {
    let mut eval= VecDeque::<usize>::new();
    let mut wt = vec![0; tree.nodes.len()];

    eval.push_front(tree.get_root());
    while !eval.is_empty() {
        let id = *eval.front().unwrap();

        if !tree.nodes[id].children.is_empty() && wt[tree.nodes[id].children[0]] == 0 {
            tree.nodes[id].children.iter()
                .for_each(|x| eval.push_front(*x));

            continue;
        }

        wt[id] = tree.nodes[id].weight +
                 tree.nodes[id].children.iter().map(|x| wt[*x]).sum::<u32>();
        eval.pop_front();
    }

    wt
}