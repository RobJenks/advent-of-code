package day7

import (
	"fmt"
	"strconv"
	"strings"

	"../common"
)

// Day7 : Solutions
func Day7() {
	fmt.Println("Part 1 result:", part1())
	fmt.Println("Part 2 result:", part2())
}

func part1() signal {
	lines := common.GetLines(common.ReadFile("day7/input.txt"))
	graph := processGraph(lines)

	return graph["a"].value
}

func part2() signal {
	lines := common.GetLines(common.ReadFile("day7/input.txt"))

	for i, line := range lines {
		if line[len(line)-4:] == "-> b" {
			lines[i] = "46065 -> b"
		}
	}

	graph := processGraph(lines)
	return graph["a"].value
}

func processGraph(lines []string) map[string]*node {
	model := parseInput(lines)

	name, wset := "", model.workingSet
	for len(*wset) != 0 {
		name = setIterator(*wset)
		delete(*wset, name)

		node := (*model.nodes)[name]
		if node.evaluated {
			panic("Working set node has already been evaluated")
		}

		// If we are missing graph predecessors then push all unsatisfied
		// dependencies into the working set and skip
		pending := getUnsatisfiedDependencies(node, model.nodes)
		if len(pending) != 0 {
			for _, x := range pending {
				(*wset)[x] = struct{}{}
			}
			continue
		}

		// Evaluate the node since all dependencies are satisfied
		node.setValue(evaluateNode(node, model.nodes))

		// Add all successor nodes to the working set
		if succ, ok := (*model.forwardDepend)[node.name]; ok {
			for _, x := range succ {
				(*wset)[x] = struct{}{}
			}
		}
	}

	return *model.nodes
}

func evaluateNode(n *node, nodes *map[string]*node) signal {
	r := n.pred
	vals := []signal{}

	// Collect input operands from predecessor nodes
	for _, x := range []string{r.a, r.b} {
		if x == "" {
			continue
		} else if isNumeric(x) {
			v, _ := strconv.ParseInt(x, 10, 0)
			vals = append(vals, signal(v))
		} else {
			nd := (*nodes)[x]
			if !nd.evaluated {
				panic("Dependency should be satisfied")
			}
			vals = append(vals, nd.value)
		}
	}

	switch r.op {
	case "SET":
		return vals[0]
	case "AND":
		return vals[0] & vals[1]
	case "OR":
		return vals[0] | vals[1]
	case "NOT":
		return ^vals[0]
	case "LSHIFT":
		return vals[0] << vals[1]
	case "RSHIFT":
		return vals[0] >> vals[1]
	default:
		panic("Unknown operation")
	}
}

func getUnsatisfiedDependencies(n *node, nodes *map[string]*node) []string {
	dep := []string{}
	for _, d := range []string{n.pred.a, n.pred.b} {
		if d == "" || isNumeric(d) {
			continue
		}

		pred := (*nodes)[d]
		if !pred.evaluated {
			dep = append(dep, pred.name)
		}
	}

	return dep
}

type signal uint16

//const noSignal signal = 0xFFFF

type node struct {
	name      string
	value     signal
	pred      rule
	evaluated bool
}

type rule struct {
	op   string
	a, b string
	tgt  string
}

type model struct {
	nodes         *map[string]*node    // Name -> Node
	forwardDepend *map[string][]string // Node -> []successors
	workingSet    *map[string]struct{} // Initial working set
}

func (n *node) setValue(value signal) {
	n.value = value
	n.evaluated = true
}

func parseInput(lines []string) model {
	nodes := make(map[string]*node)
	forwardDepend := make(map[string][]string)
	workingSet := make(map[string]struct{})
	m := model{&nodes, &forwardDepend, &workingSet}

	for _, line := range lines {
		comp := strings.Split(line, " ")

		// To allow consistent processing of all ops
		if len(comp) == 3 {
			comp = append([]string{"", "SET"}, comp...)
		} else if len(comp) == 4 {
			comp = append([]string{""}, comp...)
		}

		tgt := comp[len(comp)-1]
		r := rule{comp[1], comp[0], comp[2], tgt}
		n := node{tgt, 0, r, false}

		nodes[tgt] = &n

		hasDepends := false
		for _, x := range []string{r.a, r.b} {
			if x != "" && !isNumeric(x) {
				hasDepends = true
				forwardDepend[x] = append(forwardDepend[x], tgt)
			}
		}

		if !hasDepends {
			workingSet[tgt] = struct{}{}
		}
	}

	return m
}

func isNumeric(s string) bool {
	_, err := strconv.ParseInt(s, 10, 0)
	return err == nil
}

func setIterator(s map[string]struct{}) string {
	for k := range s {
		return k
	}

	panic("Empty set")
}
