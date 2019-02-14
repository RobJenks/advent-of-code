from unittest import TestCase

def input():
    return 3004953

def run():
    print("Part 1 result:", part1())
    print("Part 2 result:", part2())


def part1():
    return [x[0] for x in evaluate(input())][-1][1]


def part2():
    return evaluate_cross(input())


def evaluate(n):
    x, res = [(1, i) for i in range(1, n + 1)], []
    active = -1

    while len(x) != 1:
        # Single pass through the data
        for (v, ix) in x:
            if v == 0: continue

            if active == -1:
                res.append((v, ix))
                active = len(res) - 1
            else:
                tgt = (res if active < len(res) else x)
                tgt[active] = (tgt[active][0] + v, tgt[active][1])
                active = -1

        # Collapse after each pass to reduce the search space
        x = res[:]
        res = []
        yield x

        # Support carry-over across the list end boundary
        if active != -1:
            active = len(x) - 1


def evaluate_cross(n):
    x = list(map(Elf, range(n)))
    for i in range(n):
        x[i].nxt = x[(i+1) % n]
        x[i].prv = x[(i-1) % n]

    start = x[0]
    mid = x[int(n/2)]

    for i in range(n-1):
        mid.delete()

        # Move to the correct opposite-point
        mid = mid.nxt
        if (n-i) % 2 == 1:
            mid = mid.nxt

        start = start.nxt

    return start.id + 1


class Elf:
    def __init__(self, id):
        self.id = id
        self.nxt = None
        self.prv = None

    def delete(self):
        self.prv.nxt = self.nxt
        self.nxt.prv = self.prv


class Tests(TestCase):
    def test_evaluation(self):
        self.assertEqual((5, 3), [x[0] for x in evaluate(5)][-1])

    def test_cross_evaluation(self):
        self.assertEqual(2, evaluate_cross(5))
