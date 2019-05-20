package day8

import (
	"testing"

	"../common"
)

// TestCommands : Unit test
func TestUnescape(t *testing.T) {
	common.AssertEq(t, 2, sizeDelta(`""`))
	common.AssertEq(t, 2, sizeDelta(`"abc"`))
	common.AssertEq(t, 3, sizeDelta(`"aaa\"aaa"`))
	common.AssertEq(t, 5, sizeDelta(`"\x27"`))
}
