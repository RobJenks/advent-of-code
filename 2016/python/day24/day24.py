import sys
from itertools import permutations
from typing import List, Tuple
from common import io

def run():
    print("Part 1 result:", part1())
    print("Part 2 result:", part2())


def part1():
    conn = determine_connectivity(parse_input("day24/input.txt"))
    _, cost = determine_best_path(conn, False)
    return cost


def part2():
    conn = determine_connectivity(parse_input("day24/input.txt"))
    _, cost = determine_best_path(conn, True)
    return cost


def parse_input(path):
    return [[int(x) if is_location(x) else
             WALL if x == '#' else EMPTY
                for x in list(x)] for x in io.read_file(path).splitlines(False)]


def determine_connectivity(grid):
    # Find the position of all named locations in the grid
    pos = {}
    for ri, row in enumerate(grid):
        for x in [(int(row[i]), i, ri) for i, x in enumerate(row) if x >= 0]:
            pos[x[0]] = x[1:]

    # Perform pathfinding from each named location to the rest of the grid
    return [perform_pathfinding(grid, i, pos) for i in range(max(pos.keys()) + 1)]


def perform_pathfinding(grid, start, locations):
    dist = [[sys.maxsize for _ in range(len(grid[0]))] for _ in range(len(grid))]
    prev = [[(sys.maxsize, sys.maxsize) for _ in range(len(grid[0]))] for _ in range(len(grid))]
    start_pos = locations[start]

    eval = [start_pos]
    dist[start_pos[1]][start_pos[0]] = 0
    prev[start_pos[1]][start_pos[0]] = (-1, -1)

    while eval:
        pos = eval.pop()
        adj = get_adjacent(grid, pos)

        newdist = dist[pos[1]][pos[0]] + 1
        for x in adj:
            if newdist < dist[x[1]][x[0]]:
                dist[x[1]][x[0]] = newdist
                prev[x[1]][x[0]] = pos
                eval.append(x)

    # Return the distance to all named locations in the grid
    return [dist[y][x] for (i, (x, y)) in sorted(locations.items())]


def determine_best_path(conn, return_to_start):
    perm = [[0] + list(x) + ([0] if return_to_start else []) for x in permutations(range(1, len(conn)))]
    best, best_index = sys.maxsize, -1
    for i, p in enumerate(perm):
        cost = sum(conn[px][p[pi+1]] for pi, px in enumerate(p[:-1]))
        if cost < best:
            best, best_index = cost, i

    return perm[best_index], best


def get_adjacent(grid, pos) -> List[Tuple[int, int]]:
    indices = [(pos[0]+x[0], pos[1]+x[1]) for x in [(-1, 0), (0, -1), (1, 0), (0, 1)]]
    return [x for x in indices if valid_pos(grid, x) and can_traverse(grid[x[1]][x[0]])]


def valid_pos(grid, pos):
    return 0 <= pos[0] < len(grid[0]) and 0 <= pos[1] < len(grid)


# 0 = start, 1-n = target locations, ...
WALL = -1
EMPTY = WALL - 1


def is_wall(x):
    return x == WALL


def can_traverse(x):
    return not is_wall(x)


def is_location(x):
    return x.isnumeric()
