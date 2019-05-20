package day9

import (
	"testing"

	"../common"
)

// TestDistance : Unit test
func TestDistance(t *testing.T) {
	lines := []string{
		"London to Dublin = 464",
		"London to Belfast = 518",
		"Dublin to Belfast = 141",
	}

	nodes := parseInput(lines)

	common.AssertEq(t, 605, bestDistance(nodes, func() func(int) bool {
		best := int(1e6)
		return func(x int) bool {
			if x < best {
				best = x
				return true
			}
			return false
		}
	}))
}
