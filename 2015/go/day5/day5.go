package day5

import (
	"fmt"
	"math"

	"../common"
)

// Day5 : Solutions
func Day5() {
	fmt.Println("Part 1 result:", part1())
	fmt.Println("Part 2 result:", part2())
}

func part1() int {
	lines := common.GetLines(common.ReadFile("day5/input.txt"))
	return predicateOverLines(&lines, isGood)
}

func part2() int {
	lines := common.GetLines(common.ReadFile("day5/input.txt"))
	return predicateOverLines(&lines, isBetter)
}

func predicateOverLines(lines *[]string, f func(string) bool) int {
	acc := 0

	for _, line := range *lines {
		if f(line) {
			acc++
		}
	}

	return acc
}

var vowelRunes = []rune{'a', 'e', 'i', 'o', 'u'}
var badStrings = []string{"ab", "cd", "pq", "xy"}

func isGood(s string) bool {
	vowels, repeats := 0, 0
	str := (s + "#") // Append to allow 0..n for all tests

	for i, ch := range str[:len(str)-1] {
		pair := str[i : i+2]
		if containsStr(&badStrings, pair) {
			return false
		}

		if containsRune(&vowelRunes, ch) {
			vowels++
		}

		if pair[0] == pair[1] {
			repeats++
		}
	}

	return (vowels >= 3 && repeats != 0)
}

func isBetter(s string) bool {
	hasSplitRepeat := false               // x_x for any x
	repeatedPairs := make(map[string]int) // xx for any x
	str := ("@#" + s + "#@")              // Append to allow 0..n for all tests

	for i := 2; i < len(str)-2; i++ {
		if str[i] == str[i+2] {
			hasSplitRepeat = true
		}

		if str[i] == str[i+1] && str[i-1] == str[i] && str[i-2] != str[i] {
			continue // Overlapping pair; do not record
		}

		repeatedPairs[str[i:i+2]]++
	}

	var highestFreqRepeat float64
	for _, v := range repeatedPairs {
		highestFreqRepeat = math.Max(highestFreqRepeat, float64(v))
	}

	return (int(highestFreqRepeat) >= 2 && hasSplitRepeat)
}

// No generics in Go -> junk like this
func containsStr(array *[]string, str string) bool {
	for _, s := range *array {
		if s == str {
			return true
		}
	}
	return false
}

// No generics in Go -> junk like this
func containsRune(array *[]rune, rn rune) bool {
	for _, r := range *array {
		if r == rn {
			return true
		}
	}

	return false
}
