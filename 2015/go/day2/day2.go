package day2

import (
	"fmt"
	"sort"
	"strconv"
	"strings"

	"../common"
)

// Day2 : Solutions
func Day2() {
	fmt.Println("Part 1 result:", part1())
}

func part1() int {
	pres := parseInput(common.ReadFile("day2/input.txt"))

	paper := 0
	for _, p := range pres {
		paper += p.paperRequired()
	}

	return paper
}

type present struct {
	x, y, z int
}

func (p present) paperRequired() int {
	return (2 * p.x * p.y) +
		(2 * p.y * p.z) +
		(2 * p.z * p.x) +
		p.smallestSideArea()
}

func (p present) smallestSideArea() int {
	dims := []int{p.x, p.y, p.z}
	sort.Ints(dims)

	return dims[0] * dims[1]
}

func parsePresent(s string) present {
	comp := strings.Split(s, "x")
	getVal := func(s string) int {
		val, err := strconv.ParseInt(s, 10, 0)
		if err != nil {
			panic("Conversion failed")
		}
		return int(val)
	}
	return present{getVal(comp[0]), getVal(comp[1]), getVal(comp[2])}
}

func parseInput(input string) []present {
	lines := common.GetLines(input)
	pres := []present{}

	for _, line := range lines {
		pres = append(pres, parsePresent(line))
	}

	return pres
}
