package main

import (
	"fmt"

	"./day1"
	"./day10"
	"./day2"
	"./day3"
	"./day4"
	"./day5"
	"./day6"
	"./day7"
	"./day8"
	"./day9"
)

func main() {
	solutions := []func(){
		day1.Day1, day2.Day2, day3.Day3, day4.Day4, day5.Day5,
		day6.Day6, day7.Day7, day8.Day8, day9.Day9, day10.Day10,
	}

	for i, soln := range solutions {
		fmt.Printf("\nDay %d:\n", i+1)
		soln()
	}
}
