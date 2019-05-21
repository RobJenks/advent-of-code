package day12

import (
	"encoding/json"
	"fmt"

	"../common"
)

type jsonObject = map[string]interface{}

// Day12 : Solutions
func Day12() {
	fmt.Println("Part 1 result:", part1())
	fmt.Println("Part 2 result:", part2())
}

func part1() int {
	input := common.ReadFile("day12/input.txt")
	return accumulate(readJSON(input), func(_ string) bool { return false })
}

func part2() int {
	input := common.ReadFile("day12/input.txt")
	return accumulate(readJSON(input), func(x string) bool {
		return x == "red"
	})
}

func accumulate(jsonData interface{}, breakString func(string) bool) int {
	switch jsonData := jsonData.(type) {
	case jsonObject:
		return int(accumulateOverJSON(jsonData, breakString))
	case []interface{}:
		return int(accumulateOverJSONArray(jsonData, breakString))
	}

	panic("Unknown deserialized JSON type")
}

func accumulateOverJSON(data jsonObject, breakString func(string) bool) float64 {
	var acc float64
	for _, v := range data {
		switch v := v.(type) {
		case string:
			if breakString(v) {
				return 0
			}
		case float64:
			acc += v
		case jsonObject:
			acc += accumulateOverJSON(v, breakString)
		case []interface{}:
			acc += accumulateOverJSONArray(v, breakString)
		}
	}

	return acc
}

func accumulateOverJSONArray(data []interface{}, breakString func(string) bool) float64 {
	var acc float64

	for _, el := range data {
		switch el := el.(type) {
		case float64:
			acc += el
		case jsonObject:
			acc += accumulateOverJSON(el, breakString)
		case []interface{}:
			acc += accumulateOverJSONArray(el, breakString)
		}
	}

	return acc
}

func readJSON(data string) interface{} {
	var v interface{}

	b := []byte(data)
	json.Unmarshal(b, &v)

	return v
}
