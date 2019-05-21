package day13

import (
	"fmt"
	"strconv"
	"strings"

	"../common"
)

// Day13 : Solutions
func Day13() {
	fmt.Println("Part 1 result:", part1())
}

func part1() int {
	people := parseInput(common.GetLines(common.ReadFile("day13/input.txt")))
	return getBestCombination(people)
}

func getBestCombination(people map[string]person) int {
	names, indices := []string{}, []int{}
	for k := range people {
		names = append(names, k)
		indices = append(indices, len(indices))
	}

	n := len(names)
	permutations := common.Permutations(indices)

	best := 0
	for _, perm := range permutations {
		acc := 0
		for i, ix := range perm {
			left := people[names[ix]].preference[names[perm[(i+n-1)%n]]]
			right := people[names[ix]].preference[names[perm[(i+1)%n]]]

			acc += (left + right)
		}

		if acc > best {
			best = acc
		}
	}

	return best
}

type person struct {
	name       string         // Name of this person
	preference map[string]int // Map of neighbours, and +/- effect on this person
}

func parseInput(lines []string) map[string]person {
	data := make(map[string]person)

	for _, line := range lines {
		comp := strings.Split(line[:len(line)-1], " ")
		name, typ, amtString, target := comp[0], comp[2], comp[3], comp[10]

		amt, err := strconv.ParseInt(amtString, 10, 0)
		if err != nil {
			panic("Failed to parse input value")
		}

		if typ == "lose" {
			amt *= -1
		}

		_, ok := data[name]
		if !ok {
			data[name] = person{name: name, preference: map[string]int{}}
		}

		data[name].preference[target] = int(amt)
	}

	return data
}
