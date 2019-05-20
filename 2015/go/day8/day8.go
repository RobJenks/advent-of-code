package day8

import (
	"encoding/hex"
	"fmt"

	"../common"
)

// Day8 : Solutions
func Day8() {
	fmt.Println("Part 1 result:", part1())
	fmt.Println("Part 2 result:", part2())
}

func part1() int {
	lines := common.GetLines(common.ReadFile("day8/input.txt"))

	delta := 0
	for _, line := range lines {
		delta += sizeDelta(line, unescape)
	}

	return delta
}

func part2() int {
	lines := common.GetLines(common.ReadFile("day8/input.txt"))

	delta := 0
	for _, line := range lines {
		delta += sizeDelta(line, escape)
	}

	return -delta
}

func sizeDelta(str string, f func(string) string) int {
	return len(str) - len(f(str))
}

func unescape(str string) string {
	s := str[1 : len(str)-1]
	result := ""

	for i := 0; i < len(s); i++ {
		if s[i] == '\\' && i < len(s)-1 {
			if s[i+1] == '\\' || s[i+1] == '"' {
				result += string(s[i+1])
				i += 1
				continue
			} else if s[i+1] == 'x' && i < len(s)-3 {
				char, err := hex.DecodeString(s[i+2 : i+4])
				if err == nil { // Required to exclude non-hex chars following escape
					result += string(char)
					i += 3
					continue
				}
			}
		}
		result += string(s[i])
	}

	return result
}

func escape(str string) string {
	result := ""

	for _, ch := range str {
		if ch == '\\' || ch == '"' {
			result += ("\\" + string(ch))
		} else {
			result += string(ch)
		}
	}

	return (`"` + result + `"`)
}
