package common

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

// ReadFile : Read an entire file and return the contents as text
func ReadFile(path string) string {
	f, err := os.Open(path)
	if err != nil {
		panic(fmt.Sprintf("Error opening file \"%s\": %s", path, err.Error()))
	}

	defer f.Close()

	str := ""
	buffer := make([]byte, 128)
	for {
		n, err := f.Read(buffer)
		if n == 0 {
			return str
		}

		if err != nil {
			panic(fmt.Sprintf("Error reading data from \"%s\": %s", path, err.Error()))
		}

		str += string(buffer[0:n])
	}
}

// GetLines : Split a text block around newline characters
func GetLines(data string) []string {
	return strings.Split(data, "\n")
}

// GetChars : Split a text block into component UTF-8 chars
func GetChars(data string) []string {
	return strings.Split(data, "")
}

// ParseInt : Convenience function to parse and return an int, without any testing of
// error state.  Panics on error.  Used for known valid numeric input
func ParseInt(str string) int {
	val, err := strconv.ParseInt(str, 10, 0)
	if err != nil {
		panic("Cannot parse integer value")
	}

	return int(val)
}
