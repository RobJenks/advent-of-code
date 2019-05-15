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
	fmt.Println("Part 2 result:", part2())
}

func part1() int {
	pres := parseInput(common.ReadFile("day2/input.txt"))
	return accumulate(pres, paperRequired)
}

func part2() int {
	pres := parseInput(common.ReadFile("day2/input.txt"))
	return accumulate(pres, ribbonRequired)
}

func accumulate(pres []present, f func(*present) int) int {
	acc := 0
	for _, p := range pres {
		acc += f(&p)
	}

	return acc
}

type present struct {
	x, y, z int
}

func paperRequired(p *present) int {
	dims := p.sortedDimensions()
	return (2 * p.x * p.y) +
		(2 * p.y * p.z) +
		(2 * p.z * p.x) +
		(dims[0] * dims[1])
}

func ribbonRequired(p *present) int {
	dims := p.sortedDimensions()
	return dims[0] + dims[0] +
		dims[1] + dims[1] +
		(dims[0] * dims[1] * dims[2])
}

func (p present) sortedDimensions() []int {
	dims := []int{p.x, p.y, p.z}
	sort.Ints(dims)

	return dims
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
