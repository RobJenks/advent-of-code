package day9

import (
	"fmt"
	"strconv"
	"strings"

	"../common"
)

// Day9 : Solutions
func Day9() {
	fmt.Println("Part 1 result:", part1())
}

func part1() int {
	nodes := parseInput(common.GetLines(common.ReadFile("day9/input.txt")))

	return shortestDistance(nodes)
}

// Return the length of the shortest travelling-salesperson route.  Just brute force for
// this part even though it's O(n!), since n == 7 -> 5040
func shortestDistance(nodes map[string]node) int {
	keys, indices := []string{}, []int{}
	for k := range nodes {
		indices = append(indices, len(keys))
		keys = append(keys, k)
	}

	routes := permutations(indices)
	nodeCount := len(keys)

	bestDist := int(1e6)
	for _, route := range routes {
		dist := 0
		for i := 0; i < (nodeCount - 1); i++ {
			dist += nodes[keys[route[i]]].links[keys[route[i+1]]]
		}

		if dist < bestDist {
			bestDist = dist
		}
	}

	return bestDist
}

type node struct {
	name  string
	links map[string]int
}

func parseInput(lines []string) map[string]node {
	nodes := make(map[string]node)
	for _, line := range lines {
		comp := strings.Split(line, " ")
		from, to := comp[0], comp[2]
		dist, _ := strconv.ParseInt(comp[4], 10, 0)

		// Record in both forward and reverse direction (links are bidirectional)
		for _, names := range [][2]string{{from, to}, {to, from}} {
			src, dest := names[0], names[1]

			_, ok := nodes[src]
			if !ok {
				nodes[src] = node{name: src, links: make(map[string]int)}
			}
			nodes[src].links[dest] = int(dist)
		}
	}

	return nodes
}

// From SO (https://stackoverflow.com/a/30226442)
func permutations(arr []int) [][]int {
	var helper func([]int, int)
	res := [][]int{}

	helper = func(arr []int, n int) {
		if n == 1 {
			tmp := make([]int, len(arr))
			copy(tmp, arr)
			res = append(res, tmp)
		} else {
			for i := 0; i < n; i++ {
				helper(arr, n-1)
				if n%2 == 1 {
					tmp := arr[i]
					arr[i] = arr[n-1]
					arr[n-1] = tmp
				} else {
					tmp := arr[0]
					arr[0] = arr[n-1]
					arr[n-1] = tmp
				}
			}
		}
	}
	helper(arr, len(arr))
	return res
}
