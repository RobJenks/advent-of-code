package day6

import (
	"fmt"
	"math"
	"strconv"
	"strings"

	"../common"
)

const GRID_SIZE int = 1000

// Day6 : Solutions
func Day6() {
	fmt.Println("Part 1 result:", part1())
	fmt.Println("Part 2 result:", part2())
}

func part1() int {
	return getLit(common.GetLines(common.ReadFile("day6/input.txt")))
}

func part2() int {
	return getBrightness(common.GetLines(common.ReadFile("day6/input.txt")))
}

func getLit(lines []string) int {
	commands := parseInput(lines)

	var grid [GRID_SIZE][GRID_SIZE]int
	processCommands(&grid, commands, func(op operation, currentVal int) int {
		switch op {
		case on:
			return 1
		case off:
			return 0
		case toggle:
			return (currentVal + 1) % 2
		default:
			panic("Unknown operation type")
		}
	})

	count := 0
	for _, row := range grid {
		for _, cell := range row {
			if cell != 0 {
				count++
			}
		}
	}

	return count
}

func getBrightness(lines []string) int {
	commands := parseInput(lines)

	var grid [GRID_SIZE][GRID_SIZE]int
	processCommands(&grid, commands, func(op operation, currentVal int) int {
		switch op {
		case on:
			return currentVal + 1
		case off:
			return int(math.Max(0.0, float64(currentVal)-1.0))
		case toggle:
			return currentVal + 2
		default:
			panic("Unknown operation type")
		}
	})

	total := 0
	for _, row := range grid {
		for _, cell := range row {
			total += cell
		}
	}

	return total
}

func processCommands(
	grid *[GRID_SIZE][GRID_SIZE]int,
	cmds []command,
	stepFn func(operation, int) int) {

	for _, c := range cmds {
		for x := c.start.x; x <= c.end.x; x++ {
			for y := c.start.y; y <= c.end.y; y++ {
				grid[x][y] = stepFn(c.op, grid[x][y])
			}
		}
	}
}

type operation int

const (
	on     operation = iota
	toggle operation = iota
	off    operation = iota
)

type point struct {
	x, y int
}

type command struct {
	op         operation
	start, end point
}

func parseInput(lines []string) []command {
	cmds := []command{}
	for _, line := range lines {
		cmds = append(cmds, parseLine(line))
	}

	return cmds
}

func parseLine(line string) command {
	comp := strings.Split(line, " ")

	var op operation
	if comp[0] == "turn" {
		if comp[1] == "on" {
			op = on
		} else {
			op = off
		}
	} else if comp[0] == "toggle" {
		op = toggle
	} else {
		panic("Unknown operation type")
	}

	return command{op, parsePoint(comp[len(comp)-3]), parsePoint(comp[len(comp)-1])}
}

func parsePoint(str string) point {
	comp := strings.Split(str, ",")
	x, _ := strconv.ParseInt(comp[0], 10, 0)
	y, _ := strconv.ParseInt(comp[1], 10, 0)
	return point{int(x), int(y)}
}
