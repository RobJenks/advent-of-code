package day14

import (
	"fmt"
	"strings"

	"../common"
)

// Day14 : Solutions
func Day14() {
	fmt.Println("Part 1 result:", part1())
	fmt.Println("Part 2 result:", part2())
}

func part1() int {
	deer := parseInput(common.GetLines(common.ReadFile("day14/input.txt")))

	best := 0
	for _, d := range deer {
		best = common.IntMax(best, distanceAfterTime(d, 2503))
	}

	return best
}

func part2() int {
	deer := parseInput(common.GetLines(common.ReadFile("day14/input.txt")))
	score := accumulateScores(deer, 2503)

	bestScore := 0
	for _, sc := range score {
		if sc > bestScore {
			bestScore = sc
		}
	}

	return bestScore
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

func accumulateScores(deer map[string]reindeer, time int) []int {
	vec, state := []reindeer{}, []reindeerState{}
	for _, d := range deer {
		vec = append(vec, d)
		state = append(state, reindeerState{running: true, remaining: d.runTime})
	}

	dist, score := make([]int, len(vec)), make([]int, len(vec))

	for t := 0; t < time; t++ {
		bestDist := 0
		for i, d := range vec {
			if state[i].running {
				dist[i] += d.speed
				state[i].remaining--
				if state[i].remaining == 0 {
					state[i] = reindeerState{running: false, remaining: d.restTime}
				}
			} else {
				state[i].remaining--
				if state[i].remaining == 0 {
					state[i] = reindeerState{running: true, remaining: d.runTime}
				}
			}

			if dist[i] > bestDist {
				bestDist = dist[i]
			}
		}

		for i, d := range dist {
			if d == bestDist {
				score[i]++
			}
		}
	}

	return score
}

type reindeerState struct {
	running   bool
	remaining int
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
