package day15

import (
	"fmt"
	"math"
	"math/rand"
	"strings"

	"../common"
)

// Day15 : Solutions
func Day15() {
	fmt.Println("Part 1 result:", part1())
}

func part1() int {
	//rand.Seed(time.Now().UnixNano())
	ing := parseInput(common.GetLines(common.ReadFile("day15/input.txt")))

	return findBestScore(ing)
}

func findBestScore(ing []ingredient) int {
	best := -1
	for a := 0; a < 100; a++ {
		for b := 0; b < (100 - a); b++ {
			for c := 0; c < (100 - (a + b)); c++ {
				alloc := [4]int{a, b, c, (100 - (a + b + c))}
				best = int(math.Max(float64(best), float64(getScore(ing, alloc[:]))))
			}
		}
	}

	return best
}

func randomAllocation() []int {
	comp := [4]int{rand.Intn(50), rand.Intn(40), rand.Intn(10), 0}
	comp[3] = 100 - comp[0] - comp[1] - comp[2]

	result := []int{}
	for len(comp) != 0 {
		result = append(result, result[rand.Intn(len(comp))])
	}

	return result
}

func getScore(ingredients []ingredient, amounts []int) int {
	score := 1
	for f := 0; f < len(amounts); f++ {
		factorScore := 0
		for i, ing := range ingredients {
			factorScore += (ing.factors[i] * amounts[i])
		}

		score *= int(math.Max(0, float64(factorScore)))
	}

	return score
}

type ingredient struct {
	factors  [4]int
	calories int
}

func parseInput(lines []string) []ingredient {
	ing := []ingredient{}
	for _, line := range lines {
		comp := strings.Split(line, " ")
		ing = append(ing, ingredient{
			factors: [4]int{
				common.ParseInt(tidy(comp[2])),
				common.ParseInt(tidy(comp[4])),
				common.ParseInt(tidy(comp[6])),
				common.ParseInt(tidy(comp[8])),
			},
			calories: common.ParseInt(tidy(comp[10])),
		})
	}

	return ing
}

func tidy(str string) string {
	return strings.TrimSpace(strings.ReplaceAll(str, ",", ""))
}
