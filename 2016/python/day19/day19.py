from unittest import TestCase

def input():
    return 3004953

def run():
    print("Part 1 result:", part1())


def part1():
    return [x[0] for x in evaluate(input())][-1]


def evaluate(n):
    x, res = [(1, i + 1) for i in range(n)], []
    active = -1

    while len(x) != 1:
        # Single pass through the data
        for (i, (v, ix)) in enumerate(x):
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


class Tests(TestCase):
    def test_evaluation(self):
        self.assertEqual((5, 2), [x[0] for x in evaluate(5)][-1])
