package day6

import (
	"testing"

	"../common"
)

// TestCommands : Unit test
func TestCommands(t *testing.T) {
	common.AssertEq(t, 1000000, getLit(common.GetLines("turn on 0,0 through 999,999")))
	common.AssertEq(t, 1000, getLit(common.GetLines("toggle 0,0 through 999,0")))
	common.AssertEq(t, 4, getLit(common.GetLines("turn on 499,499 through 500,500")))
	common.AssertEq(t, 0, getLit(common.GetLines("turn off 499,499 through 500,500")))
}

func mockCommands(str string) []command {
	return parseInput(common.GetLines(str))
}
