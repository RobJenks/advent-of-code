from common import io

def run():
    print("Part 1 result:", part1())
    print("Part 2 result:", part2())


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


def part2():
    # Solved manually (as expected by problem description) with the following:
    #    print(render_grid(build_grid(parse_input(io.read_file("day22/input.txt")))))

    return (10 +        # Moves of empty data node to left of high-capacity wall
            6 +         # Moves of empty node directly up to (x = 0)
            25 +        # Moves to empty node to immediately left of goal node
            (32 * 5) +  # Move of goal node 32 spaces left, * 5 moves required at each step to cycle around empty node
            1)          # Final move of goal node into empty node now at (0, 0)


def parse_input(input):
    id, res = 0, []
    for x in [x.split() for x in input.splitlines(False)][2:]:
        key = x[0].split('-')
        pos = (int(key[1][1:]), int(key[2][1:]))
        res.append((id, pos, int(x[1][:-1]), int(x[2][:-1]), int(x[3][:-1])))
        id += 1

    return res


def build_grid(nodes):
    mx = [0, 0]
    for x in nodes:
        mx = [max(mx[i], x[1][i]) for i in range(2)]

    grid = [[None for _ in range(mx[0]+1)] for _ in range(mx[1]+1)]
    for x in nodes:
        grid[x[1][1]][x[1][0]] = x

    return grid


def render_grid(grid):
    st = []
    for (r, row) in enumerate(grid):
        st += ['_' if used(x) == 0 else
               '#' if used(x) > 100 else
               'S' if (r, i) == (0, 0) else
               'G' if (r, i) == (0, len(row)-1) else
               '.' for (i, x) in enumerate(row)] + ['\n']

    return ''.join(st)



def size(t): return t[2]
def used(t): return t[3]
def free(t): return t[4]
