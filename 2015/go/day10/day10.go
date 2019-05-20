package day10

import (
	"fmt"
	"strconv"
)

const input string = "3113322113"

// Day10 : Solutions
func Day10() {
	fmt.Println("Part 1 result:", part1())
	fmt.Println("Part 2 result:", part2())
}

func part1() int {
	return len(iterativeTransform(input, 40))
}

func part2() int {
	return len(iterativeTransform(input, 50))
}

func iterativeTransform(input string, iterations int) string {
	str := input
	for i := 0; i < iterations; i++ {
		str = transform(str)
	}

	return str
}

func transform(input string) string {
	current, length := input[0], 1
	next := func() string { return (strconv.Itoa(length) + string(current)) }
	result := ""

	for i := 1; i < len(input); i++ {
		if input[i] == current {
			length++
		} else {
			result += next()
			current, length = input[i], 1
		}
	}
	result += next()

	return result
}
