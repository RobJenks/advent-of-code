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
	fmt.Println("Part 2 result:", part2())
}

func part1() int {
	return firstHashWithPrefix("00000")
}

func part2() int {
	return firstHashWithPrefix("000000")
}

func firstHashWithPrefix(prefix string) int {
	pn := len(prefix)
	for i := 0; ; i++ {
		h := hash(fmt.Sprintf("%s%d", input, i))
		if h[:pn] == prefix {
			return i
		}
	}
}

func hash(s string) string {
	b := md5.Sum([]byte(s))
	return hex.EncodeToString(b[:])
}
