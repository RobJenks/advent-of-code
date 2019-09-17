package day15

import (
	"testing"

	"../common"
)

// TestScore : Unit test
func TestScore(t *testing.T) {
	ing := parseInput([]string{
		"Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8",
		"Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3",
	})

	common.AssertEq(t, 62842880, getScore(ing, []int{44, 56}))
}

// TestBestScore : Unit test
func TestBestScore(t *testing.T) {
	ing := parseInput([]string{
		"Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8",
		"Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3",
	})

	common.AssertEq(t, 62842880, findBestScore(ing))
}
