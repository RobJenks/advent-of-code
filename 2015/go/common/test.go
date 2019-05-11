package common

import (
	"fmt"
	"testing"
)

// AssertEqS : Assert equality, or fail with the given assertion failure message
func AssertEqS(t *testing.T, exp interface{}, act interface{}, assertionErrorMsg string) {
	if exp != act {
		fmt.Println("> Assertion failure")
		fmt.Println("Expected:", exp)
		fmt.Println("Actual  :", act)

		t.Fatalf("Assertion failure: %s", assertionErrorMsg)
	}
}

// AssertEq : Assert equality, or otherwise fail
func AssertEq(t *testing.T, exp interface{}, act interface{}) {
	AssertEqS(t, exp, act, "Values are not equal")
}

// AssertTrueS : Assert that the given expression evaluates to true, or fail with the given assertion failure message
func AssertTrueS(t *testing.T, val interface{}, assertionErrorMsg string) {
	AssertEqS(t, true, val, assertionErrorMsg)
}

// AssertTrue : Assert that the given expression evaluates to true, or otherwise fail
func AssertTrue(t *testing.T, val interface{}) {
	AssertTrueS(t, val, "Expression does not evaluate to true")
}
