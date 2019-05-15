package day1

import (
	"fmt"
	"math"

	"../common"
)

// Day1 : Solutions
func Day1() {
	fmt.Println("Part 1 result:", part1())
	fmt.Println("Part 2 result:", part2())
}

func part1() int {
	return evaluate(common.ReadFile("day1/input.txt"))
}

func part2() int {
	return evaluateToFloor(common.ReadFile("day1/input.txt"), -1)
}

func evaluate(data string) int {
	// Not at all necessary, but testing out goroutines/channels
	cN := int(math.Min(10, float64(len(data))))
	channels := []chan int{}
	for i := 0; i < cN; i++ {
		channels = append(channels, make(chan int))
		go getLevelChange(data[i*(len(data)/cN):(i+1)*(len(data)/cN)], channels[i])
	}

	level := 0
	for _, ch := range channels {
		level += <-ch
	}

	return level
}

func evaluateToFloor(data string, floor int) int {
	f := 0
	for i, ch := range data {
		if ch == '(' {
			f++
		} else {
			f--
		}

		if f == floor {
			return i + 1
		}
	}

	panic("Failed to reach target")
}

func getLevelChange(data string, ch chan int) {
	level := 0
	for _, ch := range data {
		switch ch {
		case '(':
			level++
		case ')':
			level--
		}
	}

	ch <- level
}
