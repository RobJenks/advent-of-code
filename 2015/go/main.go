package main

import (
	"fmt"

	"./day1"
	"./day2"
	"./day3"
	"./day4"
	"./day5"
	"./day6"
)

func main() {
	solutions := []func(){
		day1.Day1, day2.Day2, day3.Day3, day4.Day4, day5.Day5,
		day6.Day6,
	}

	for i, soln := range solutions {
		fmt.Printf("\nDay %d:\n", i+1)
		soln()
	}
}
