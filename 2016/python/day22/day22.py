from common import io

def run():
    print("Part 1 result:", part1())


def part1():
    data = parse_input(io.read_file("day22/input.txt"))
    by_used = sorted(data, key=lambda t: used(t))
    by_free = sorted(data, key=lambda t: free(t), reverse=True)

    count = 0
    for x in [x for x in by_used if used(x) != 0]:
        for y in by_free:
            if used(x) > free(y): break
            if x[0] != y[0]:
                count += 1

    return count


def parse_input(input):
    id, res = 0, []
    for x in [x.split() for x in input.splitlines(False)][2:]:
        key = x[0].split('-')
        pos = (int(key[1][1:]), int(key[2][1:]))
        res.append((id, pos, int(x[1][:-1]), int(x[2][:-1]), int(x[3][:-1])))
        id += 1

    return res


def size(t): return t[2]
def used(t): return t[3]
def free(t): return t[4]
