package day14

import (
	"fmt"
	"strings"

	"../common"
)

// Day14 : Solutions
func Day14() {
	fmt.Println("Part 1 result:", part1())
}

func part1() int {
	deer := parseInput(common.GetLines(common.ReadFile("day14/input.txt")))

	best := 0
	for _, d := range deer {
		best = common.IntMax(best, distanceAfterTime(d, 2503))
	}

	return best
}

type reindeer struct {
	name           string
	speed, runTime int
	restTime       int
}

func distanceAfterTime(deer reindeer, time int) int {
	unitTime := (deer.runTime + deer.restTime)
	wholeUnits := time / unitTime
	remainingSecs := time - (wholeUnits * unitTime)

	return (wholeUnits * (deer.runTime * deer.speed)) +
		(common.IntMin(remainingSecs, deer.runTime) * deer.speed)
}

func parseInput(lines []string) map[string]reindeer {
	deer := make(map[string]reindeer)
	for _, line := range lines {
		comp := strings.Split(line, " ")
		deer[comp[0]] = reindeer{
			name:     comp[0],
			speed:    common.ParseInt(comp[3]),
			runTime:  common.ParseInt(comp[6]),
			restTime: common.ParseInt(comp[13]),
		}
	}

	return deer
}
