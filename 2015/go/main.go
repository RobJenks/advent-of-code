package main

import (
	"fmt"

	"./day1"
	"./day2"
	"./day3"
)

func main() {
	solutions := []func(){
		day1.Day1, day2.Day2, day3.Day3,
	}

	for i, soln := range solutions {
		fmt.Printf("\nDay %d:\n", i+1)
		soln()
	}
}
