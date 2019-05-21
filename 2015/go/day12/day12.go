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
}

func part1() int {
	input := common.ReadFile("day12/input.txt")
	return accumulate(readJSON(input))
}

func accumulate(jsonData interface{}) int {
	switch jsonData := jsonData.(type) {
	case jsonObject:
		return int(accumulateOverJSON(jsonData))
	case []interface{}:
		return int(accumulateOverJSONArray(jsonData))
	}

	panic("Unknown deserialized JSON type")
}

func accumulateOverJSON(data jsonObject) float64 {
	var acc float64
	for _, v := range data {
		switch v := v.(type) {
		case float64:
			acc += v
		case jsonObject:
			acc += accumulateOverJSON(v)
		case []interface{}:
			acc += accumulateOverJSONArray(v)
		}
	}

	return acc
}

func accumulateOverJSONArray(data []interface{}) float64 {
	var acc float64

	for _, el := range data {
		switch el := el.(type) {
		case float64:
			acc += el
		case jsonObject:
			acc += accumulateOverJSON(el)
		case []interface{}:
			acc += accumulateOverJSONArray(el)
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
