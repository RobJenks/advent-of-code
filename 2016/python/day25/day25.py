from itertools import islice, count
from day12 import day12

def run():
    print("Part 1 result:", part1())


def part1():
    inst = day12.parse_input("day25/input.txt")

    for i in count():
        expected = False
        for ix, (_, _, x) in enumerate(day12.execute_to_output(inst, [i, 0, 0, 0])):
            if x[-1] != expected: break
            expected = not expected

            if ix > 100:
                return i

    raise RuntimeError("No solution in expected bounds")
