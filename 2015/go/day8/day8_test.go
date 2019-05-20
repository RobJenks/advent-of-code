package day8

import (
	"testing"

	"../common"
)

// TestUnescape : Unit test
func TestUnescape(t *testing.T) {
	common.AssertEq(t, 2, sizeDelta(`""`, unescape))
	common.AssertEq(t, 2, sizeDelta(`"abc"`, unescape))
	common.AssertEq(t, 3, sizeDelta(`"aaa\"aaa"`, unescape))
	common.AssertEq(t, 5, sizeDelta(`"\x27"`, unescape))
}
