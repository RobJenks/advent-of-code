package day5

import (
	"testing"

	"../common"
)

// TestBetterCriteria : Unit test
func TestBetterCriteria(t *testing.T) {
	common.AssertTrue(t, isBetter("qjhvhtzxzqqjkmpb"))
	common.AssertTrue(t, isBetter("xxyxx"))
	common.AssertFalse(t, isBetter("uurcxstgmygtbstg"))
	common.AssertFalse(t, isBetter("ieodomkazucvgmuy"))
}
