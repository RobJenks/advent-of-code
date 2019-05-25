package day14

import (
	"testing"

	"../common"
)

// TestDistance : Unit test
func TestDistance(t *testing.T) {
	deer := parseInput([]string{
		"Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.",
		"Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds.",
	})

	common.AssertEq(t, 14, distanceAfterTime(deer["Comet"], 1))
	common.AssertEq(t, 16, distanceAfterTime(deer["Dancer"], 1))
	common.AssertEq(t, 140, distanceAfterTime(deer["Comet"], 10))
	common.AssertEq(t, 160, distanceAfterTime(deer["Dancer"], 10))
	common.AssertEq(t, 140, distanceAfterTime(deer["Comet"], 11))
	common.AssertEq(t, 176, distanceAfterTime(deer["Dancer"], 11))
	common.AssertEq(t, 1120, distanceAfterTime(deer["Comet"], 1000))
	common.AssertEq(t, 1056, distanceAfterTime(deer["Dancer"], 1000))
}
