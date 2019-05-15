package day4

import (
	"crypto/md5"
	"encoding/hex"
	"fmt"
)

const input string = "iwrupvqb"

// Day4 : Solutions
func Day4() {
	fmt.Println("Part 1 result:", part1())
}

func part1() int {
	for i := 0; ; i++ {
		h := hash(fmt.Sprintf("%s%d", input, i))
		if h[:5] == "00000" {
			return i
		}
	}
}

func hash(s string) string {
	b := md5.Sum([]byte(s))
	return hex.EncodeToString(b[:])
}
