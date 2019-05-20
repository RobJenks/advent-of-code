package da9

import (
	"fmt"
)

// Day9 : Solutions
func Day9() {
	fmt.Println("Part 1 result:", part1())
}

func part1() int {
	return 12
}

type node struct {
	name  string
	links []link
}

type link struct {
	target string
	dist   int
}

func parseInput(lines []string) map[string]node {
	
}
