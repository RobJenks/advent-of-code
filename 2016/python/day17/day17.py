from unittest import TestCase
from hashlib import md5


def input():
    return "dmypynyp"


def run():
    print("Part 1 result:", part1())
    print("Part 2 result:", part2())


def part1():
    return min(pathfind(input(), 20), key=len)


def part2():
    return max(len(x) for x in pathfind(input(), 1000))


def pathfind(input, max_depth):
    res = []
    eval = [((0, 0), '')]

    while eval:
        pos, path = eval.pop()
        if len(path) == max_depth:
            continue

        opts = get_options(input, path)
        for opt in opts:
            newpos = (pos[0] + opt[1][0], pos[1] + opt[1][1])
            if newpos == (3, 3):
                res.append(path + opt[0])
            elif valid_pos(newpos):
                eval.append((newpos, path + opt[0]))

    if not res:
        raise RuntimeError("No solution within maximum search space depth")

    return res


def get_options(input, path):
    hash = md5((input + path).encode()).hexdigest()
    return [DIRS[i] for i in range(4) if is_open(hash[i])]


def is_open(ch):
    return ch == 'b' or ch == 'c' or ch == 'd' or ch == 'e' or ch == 'f'


def valid_pos(pos):
    return 0 <= pos[0] < 4 and 0 <= pos[1] < 4


DIRS = [('U', (0, -1)), ('D', (0, 1)), ('L', (-1, 0)), ('R', (1, 0))]


class Tests(TestCase):
    def test_pathfinding(self):
        self.assertEqual('DDRRRD', min(pathfind('ihgpwlah', 10), key=len))
        self.assertEqual('DDUDRLRRUDRD', min(pathfind('kglvqrro', 20), key=len))
        self.assertEqual('DRURDRUDDLLDLUURRDULRLDUUDDDRR', min(pathfind('ulqzkmiv', 36), key=len))


    def test_longest_routes(self):
        self.assertEqual(370, max(len(x) for x in pathfind('ihgpwlah', 400)))
        self.assertEqual(492, max(len(x) for x in pathfind('kglvqrro', 600)))
        self.assertEqual(830, max(len(x) for x in pathfind('ulqzkmiv', 1000)))
