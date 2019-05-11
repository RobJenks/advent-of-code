package main

import (
	"fmt"

	"./day1"
)

func main() {
	solutions := []func(){
		day1.Day1,
	}

	for i, soln := range solutions {
		fmt.Printf("\nDay %d:\n", i+1)
		soln()
	}
}
