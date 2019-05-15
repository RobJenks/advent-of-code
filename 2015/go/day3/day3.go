package day3

import (
	"fmt"

	"../common"
)

// Day3 : Solutions
func Day3() {
	fmt.Println("Part 1 result:", part1())
}

func part1() int {
	visits := getVisits(common.ReadFile("day3/input.txt"))
	return len(visits)
}

func getVisits(input string) map[[2]int]int {
	visited := make(map[[2]int]int)
	pos := [2]int{0, 0}

	for _, ch := range input {
		visited[pos]++
		pos = addCoords(pos, delta[ch])
	}

	return visited
}

func addCoords(a, b [2]int) [2]int {
	return [2]int{a[0] + b[0], a[1] + b[1]}
}

var delta = map[rune][2]int{
	'<': {-1, 0},
	'^': {0, 1},
	'>': {1, 0},
	'v': {0, -1},
}
