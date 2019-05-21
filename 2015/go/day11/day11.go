package day11

import (
	"fmt"
)

const input string = "hxbxwxba"

// Day11 : Solutions
func Day11() {
	fmt.Println("Part 1 result:", part1())
}

func part1() string {
	return nextPassword(input)
}

func nextPassword(str string) string {
	b := []byte(str)

	// Increment to the first string without invalid chars
	for containsInvalidChar(b) {
		increment(&b)
	}

	for {
		inc := increment(&b)

		// If we increment a char into an invalid one, perform a direct increment
		// to it again to avoid incrementing all lower combinations
		if isInvalidChar(b[inc]) {
			incrementAt(&b, inc)
		}

		if isValidPassword(b) {
			return string(b)
		}
	}
}

func increment(data *[]byte) int {
	return incrementAt(data, len(*data)-1)
}

func incrementAt(data *[]byte, activeIndex int) int {
	for i := activeIndex; i >= 0; i-- {
		x := (*data)[i]
		if x != 'z' {
			(*data)[i]++
			return i
		}

		(*data)[i] = 'a'
	}

	panic("Overflow in fixed-length string")
}

func isInvalidChar(x byte) bool {
	return x == 'i' || x == 'o' || x == 'l'
}

func containsInvalidChar(b []byte) bool {
	for _, ch := range b {
		if isInvalidChar(ch) {
			return true
		}
	}
	return false
}

// Asserts all validity criteria, but not invalid chars (handled at a higher level)
func isValidPassword(b []byte) bool {
	n := len(b)
	havePairs, haveSeq := false, false
	var lastPair byte = 0

	for i := 1; i < n; i++ {
		if b[i] == b[i-1] {
			// Potential non-overlapping, matching pair
			if (i > 1 && b[i-2] != b[i]) ||
				(i > 2 && b[i-3] == b[i]) {
				if b[i] != lastPair {
					if lastPair != 0 {
						havePairs = true
						if haveSeq {
							return true
						}
					}
					lastPair = b[i]
				}
			}
		}
		// Triple sequence
		if b[i-1] == (b[i]-1) &&
			(i > 1 && b[i-2] == (b[i]-2)) {
			haveSeq = true
			if havePairs {
				return true
			}
		}
	}

	return false
}
