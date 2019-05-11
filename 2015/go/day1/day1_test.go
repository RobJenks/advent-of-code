package day1

import (
	"testing"

	"../common"
)

// TestEval : Unit test
func TestEval(t *testing.T) {
	common.AssertEq(t, 0, evaluate("(())"))
	common.AssertEq(t, 0, evaluate("()()"))
	common.AssertEq(t, 3, evaluate("((("))
	common.AssertEq(t, 3, evaluate("(()(()("))
	common.AssertEq(t, 3, evaluate("))((((("))
	common.AssertEq(t, -1, evaluate("())"))
	common.AssertEq(t, -1, evaluate("))("))
	common.AssertEq(t, -3, evaluate(")))"))
	common.AssertEq(t, -3, evaluate(")())())"))
}
