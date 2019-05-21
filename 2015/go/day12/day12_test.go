package day12

import (
	"testing"

	"../common"
)

// TestAccumulate : Unit test
func TestAccumulate(t *testing.T) {
	common.AssertEq(t, 6, accumulate(readJSON(`[1,2,3]`)))
	common.AssertEq(t, 6, accumulate(readJSON(`{"a":2,"b":4}`)))
	common.AssertEq(t, 3, accumulate(readJSON(`[[[3]]]`)))
	common.AssertEq(t, 3, accumulate(readJSON(`{"a":{"b":4},"c":-1}`)))
	common.AssertEq(t, 0, accumulate(readJSON(`{"a":[-1,1]}`)))
	common.AssertEq(t, 0, accumulate(readJSON(`[-1,{"a":1}]`)))
	common.AssertEq(t, 0, accumulate(readJSON(`[]`)))
	common.AssertEq(t, 0, accumulate(readJSON(`{}`)))
}
