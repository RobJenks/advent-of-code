from unittest import TestCase
from hashlib import md5


def input():
    return "dmypynyp"


def run():
    print("Part 1 result:", part1())


def part1():
    return pathfind(input(), 20)


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

    return min(res, key=len)


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
        self.assertEqual('DDRRRD', pathfind('ihgpwlah', 10))
        self.assertEqual('DDUDRLRRUDRD', pathfind('kglvqrro', 20))
        self.assertEqual('DRURDRUDDLLDLUURRDULRLDUUDDDRR', pathfind('ulqzkmiv', 36))

