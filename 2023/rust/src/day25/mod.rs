use std::collections::{HashMap, VecDeque};
use std::iter::Iterator;
use crate::common::graph::graph::{EdgeData, Graph, NodeData};
use super::common;

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> usize {
    let (part0, part1) = partition_network(&parse_input("src/day25/problem-input.txt"));
    part0.len() * part1.len()
}

fn part2() -> usize {
    12
}

type Network = Graph<String, usize, u32>;

fn partition_network(network: &Network) -> (Vec<usize>, Vec<usize>) {
    // Determine number of connection from node[0] to all other nodes
    let start = network.nodes[0].id;
    let node_count = network.nodes.len();
    let mut partitions = [vec![start], vec![]];

    for target in 1..node_count {
        let mut connections = 0usize;

        let mut used_comps = vec![false; node_count];
        used_comps[start] = true;

        for connected_comp in &network.edges[start] {
            if connected_comp.target == target {
                connections += 1;
                continue;
            }

            let mut visited = vec![false; node_count];
            let mut active = VecDeque::<(usize, Vec<usize>)>::new();
            active.push_back((start, vec![start]));
            let mut found = false;

            while !active.is_empty() && !found {
                let Some((comp, path)) = active.pop_front() else { panic!("Queue error") };
                for c in &network.edges[comp] {
                    if target == c.target {
                        connections += 1;
                        path.iter().for_each(|x| used_comps[*x] = true);
                        found = true;
                        break;
                    }
                    else if !visited[c.target] && !path.contains(&c.target) && !used_comps[c.target] {
                        let mut new_path = path.clone();
                        new_path.push(c.target);
                        active.push_back((c.target, new_path));
                        visited[c.target] = true;
                    }
                }
            }
        }

        // More than 3 fully-disjoint paths between two nodes means that they should be
        // within the same partition, once the network is partitioned by removing 3 edges
        partitions[if connections > 3 { 0 } else { 1 }].push(target);
    }

    (partitions[0].clone(), partitions[1].clone())
}

fn parse_input(file: &str) -> Network {
    let mut nodes = Vec::<NodeData<String, usize>>::new();
    let mut node_pos = HashMap::<String, usize>::new();
    let mut edges = Vec::<EdgeData<usize, u32>>::new();

    common::read_file(file).lines()
        .map(|line| line.split_once(":").unwrap_or_else(|| panic!("Invalid input format")))
        .flat_map(|(k, vs)| vs.split_ascii_whitespace().map(|v| (k.trim().to_string(), v.trim().to_string())))
        .for_each(|(n0, n1)| {
            for n in [&n0, &n1] {
                if !node_pos.contains_key(n) {
                    let ix = nodes.len();
                    nodes.push(NodeData::new(n.clone(), ix));
                    node_pos.insert(n.clone(), ix);
                }
            }

            let i0 = *node_pos.get(n0.as_str()).unwrap_or_else(|| panic!("Failed to add node"));
            let i1 = *node_pos.get(n1.as_str()).unwrap_or_else(|| panic!("Failed to add node"));

            edges.push(EdgeData::new(i0, i1, 1));
            edges.push(EdgeData::new(i1, i0, 1));
        });

    Network::new_with(nodes, edges)
}


#[cfg(test)]
mod tests {
    use crate::day25::{parse_input, part1, part2, partition_network};

    #[test]
    fn test_network_partition() {
        assert_eq!(Some(partition_network(&parse_input("src/day25/problem-input.txt")))
            .map(|(part0, part1)| part0.len() * part1.len())
            .unwrap_or_else(|| panic!("No solution")),

            543564);
    }

    #[test]
    fn test_part1() {
        assert_eq!(part1(), 543564);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(), 12);
    }
}
