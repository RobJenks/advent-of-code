package day11

import (
	"testing"

	"../common"
)

// TestIncrement : Unit test
func TestIncrement(t *testing.T) {
	seq := []string{"xx", "xy", "xz", "ya", "yb"}

	val := []byte(seq[0])
	for i := 1; i < len(seq); i++ {
		increment(&val)
		common.AssertEq(t, seq[i], string(val))
	}
}

// TestValidity : Unit test
func TestValidity(t *testing.T) {
	common.AssertFalse(t, isValid("hijklmmn"))
	common.AssertFalse(t, isValid("abbceffg"))
	common.AssertFalse(t, isValid("abbcegjk"))
	common.AssertTrue(t, isValid("abcdffaa"))
	common.AssertTrue(t, isValid("ghjaabcc"))
}

// TestGeneration : Unit test
func TestGeneration(t *testing.T) {
	common.AssertEq(t, "abcdffaa", nextPassword("abcdefgh"))
	common.AssertEq(t, "ghjaabcc", nextPassword("ghijklmn"))
}

func isValid(str string) bool {
	for _, ch := range str {
		if isInvalidChar(byte(ch)) {
			return false
		}
	}

	return isValidPassword([]byte(str))
}
