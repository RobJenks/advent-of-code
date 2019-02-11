from unittest import TestCase
import re
from enum import IntEnum
from typing import Tuple, List
from common import io

State = List[List[bool]]
Mapping = List[str]
Action = Tuple[int, int, List[int]]


def run():
    print("Part 1 result:", part1())
    print("Part 2 result:", part2())


def part1():
    (state, mapping) = parse_input("day11/input.txt")
    (_, actions) = solve(state, mapping)
    return len(actions)


def part2():
    (state, mapping) = parse_input("day11/input2.txt")
    (_, actions) = solve(state, mapping)
    return len(actions)


def solve(state: State, mapping: Mapping) -> Tuple[State, List[Action]]:
    current, eval = 0, [(state[:], [])]     # { state, prior-actions }
    hist = set()

    # Breadth-first traversal of option space, with very aggressive (and necessary) pruning
    while current < len(eval):
        (st, act) = eval[current]
        if at_goal_state(st):       # Terminate if we have reached the goal state
            return (st, act)

        options = generate_options(st, act)
        hashed = [hash_state(x[0]) for x in options]

        for i in range(len(hashed)):
            if hashed[i] not in hist:
                eval.append(options[i])
                hist.add(hashed[i])

        current += 1

    raise RuntimeError("Solver failed to reach goal state")


def at_goal_state(state: State):
    return not any([any(row) for row in state[:len(state)-1]]) \
           and all(state[-1])


def generate_options(state: State, actions):
    # We can take { one or two } items { up or down } from each state
    # Some state transitions can be immediately pruned, e.g. ones that are provably counter-productive
    options = []
    floor = [i for i in range(len(state)) if state[i][0]] [0]
    floor_items = [i for i in range(1, len(state[floor])) if state[floor][i]]

    # Move two items upstairs, if possible
    if floor != len(state) - 1:
        moved = False
        for i in range(len(floor_items)):
            for j in range(i + 1, len(floor_items)):
                act = action(floor, 1, [floor_items[i], floor_items[j]])
                st = new_state(state, act)
                if valid(st):
                    moved = True
                    options.append((st, actions + [act]))

        # If we could not move two items upstairs, try moving just one
        if not moved:
            for i in floor_items:
                act = action(floor, 1, [i])
                st = new_state(state, act)
                if valid(st):
                    options.append((st, actions + [act]))
    # }

    # We may also need to move items downstairs; try single items, first
    if floor != 0:
        moved = False
        for i in floor_items:
            act = action(floor, -1, [i])
            st = new_state(state, act)
            if valid(st):
                moved = True
                options.append((st, actions + [act]))

        # If we could not move one item down, see if it is possible to move two items down (which protect each other)
        if not moved:
            for i in range(len(floor_items)):
                for j in range(i + 1, len(floor_items)):
                    act = action(floor, -1, [floor_items[i], floor_items[j]])
                    st = new_state(state, act)
                    if valid(st):
                        options.append((st, actions + [act]))

    # }

    return options


def hash_state(state: State):
    return tuple(val for x in state for val in x)


def new_state(state: State, action: Tuple[int, int, List[int]]):
    st = [x[:] for x in state]

    # Move elevator
    st[action[0]][0] = False
    st[action[0] + action[1]][0] = True

    # Move items
    for x in action[2]:
        st[action[0]][x] = False
        st[action[0] + action[1]][x] = True

    return st


def action(floor: int, move: int, items: List[int]) -> Action:
    return (floor, move, items)


def valid(state: State):
    for floor in state:
        have_unprotected_chip, have_active_gen = False, False
        for i in range(1, len(floor), 2):
            if floor[i] and not floor[i + 1]:
                if have_active_gen: return False        # Failed: found unprotected chip on the same floor as an active generator
                have_unprotected_chip = True
            if floor[i + 1]:
                if have_unprotected_chip: return False  # Failed: found active generator on the same floor as an unprotected chip
                have_active_gen = True

    return True                                         # State is valid


def parse_input(path) -> Tuple[State, Mapping]:
    state: State = []
    mapping: List[str] = ["elevator"]
    positions = {}

    for (i, line) in enumerate(io.read_file(path).splitlines(False)):
        state.append([])
        if 'nothing relevant' not in line:
            for chip in re.findall(r"([a-z]*?)-compatible", line):
                if chip not in mapping: mapping.append(chip)
                positions[(mapping.index(chip), ObjType.Chip)] = i

            for gen in re.findall(r"([a-z]*?) generator", line):
                if gen not in mapping: mapping.append(gen)
                positions[(mapping.index(gen), ObjType.Generator)] = i

    state = [[False for _ in range(len(mapping) * 2 - 1)] for _ in range(len(state))]

    state[0][0] = True  # Elevator
    for e in positions.items():
        state[e[1]][(1 + (e[0][0] - 1) * 2) + (1 if e[0][1] == ObjType.Generator else 0)] = True

    return state, mapping


def state_str(state: State, mapping: Mapping) -> str:
    st, fmt = [], '{:<4}'
    for (i, floor) in enumerate(state):
        s = [f'F{i + 1}  ']
        s.append(fmt.format('E' if floor[0] else ' '))
        for k in range(len(mapping) - 1):
            s.append(fmt.format(f'C{k}' if floor[1 + (k*2)] else ''))
            s.append(fmt.format(f'G{k}' if floor[1 + (k*2 + 1)] else ''))

        st.append([*s, '\n'])

    return ''.join(list(reversed([''.join(x) for x in st])))


def action_str(action: Action) -> str:
    return "Move items {} {} from floor {} to floor {}"\
        .format(str(action[2]), "upstairs" if action[1] > 0 else "downstairs", action[0] + 1, action[0] + action[1] + 1)


class ObjType(IntEnum):
    Elevator = 0
    Chip = 1
    Generator = 2


class Tests(TestCase):
    def test_goal(self):
        self.assertTrue(at_goal_state([[False, False], [False, False], [False, False], [True, True]]))
        self.assertFalse(at_goal_state([[False, True], [False, False], [False, False], [True, True]]))
        self.assertFalse(at_goal_state([[False, False], [False, False], [False, False], [False, True]]))

    def test_hash(self):
        h = hash_state([[False, True], [False, False], [True, True], [True, False]])
        self.assertEqual((False, True, False, False, True, True, True, False), h)

        hashset = set()
        hashset.add(h)
        self.assertTrue((False, True, False, False, True, True, True, False) in hashset)

    def test_solver(self):
        (state, mapping) = parse_input("tests.txt")
        sol = solve(state, mapping)
        self.assertTrue(at_goal_state(sol[0]))
        self.assertEqual(11, len(sol[1]))
