from common import io

def run():
    print("Part 1 result:", part1())
    print("Part 2 result:", part2())


def part1():
    return [list(reversed(list(get_key(x, keypad=[[1, 2, 3], [4, 5, 6], [7, 8, 9]], start=[1, 1]))))[0]
            for x in parse_input("day2/input.txt")]


def part2():
    return [list(reversed(list(get_key(x, keypad=[['0', '0', '1', '0', '0'], ['0', '2', '3', '4', '0'], ['5', '6', '7', '8', '9'],
                                                  ['0', 'A', 'B', 'C', '0'], ['0', '0', 'D', '0', '0']], start=[1, 1]))))[0]
            for x in parse_input("day2/input.txt")]


def parse_input(path):
    return [list(x) for x in io.read_file(path).splitlines(False)]


def get_key(path, keypad, start):
    for ch in path:
        start = valid_key(keypad, start, DIRECTIONS[ch])
        yield keypad[start[1]][start[0]]


def valid_key(keypad, pos, dir):
    ptr = [x + y for (x, y) in zip(pos, dir)]
    return ptr if all(x >= 0 for x in ptr) and ptr[1] < len(keypad) and ptr[0] < len(keypad[ptr[1]]) and keypad[ptr[1]][ptr[0]] != '0' \
        else pos


DIRECTIONS = {'L': [-1, 0], 'U': [0, -1], 'R': [1, 0], 'D': [0, 1]}
