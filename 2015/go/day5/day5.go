package day5

import (
	"fmt"

	"../common"
)

// Day5 : Solutions
func Day5() {
	fmt.Println("Part 1 result:", part1())
}

func part1() int {
	lines := common.GetLines(common.ReadFile("day5/input.txt"))
	good := 0

	for _, line := range lines {
		if isGood(line) {
			good++
		}
	}

	return good
}

var vowelRunes = []rune{'a', 'e', 'i', 'o', 'u'}
var badStrings = []string{"ab", "cd", "pq", "xy"}

func isGood(s string) bool {
	vowels, repeats := 0, 0
	str := (s + ".") // Append to allow 0..n for all tests

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
