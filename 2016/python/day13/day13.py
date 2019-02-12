import sys
from unittest import TestCase
from typing import List, Tuple

def input():
    return 1364

def run():
    print("Part 1 result:", part1(input()))


def part1(input):
    return distance_map(generate(40, 50, input), (1, 1))[39][31]


def generate(width, height, seed):
    return [[is_wall(x, y, seed) for x in range(width)] for y in range(height)]


def is_wall(x, y, seed):
    return sum(1 for d in format(seed + x*x + 3*x + 2*x*y + y + y*y, 'b') if d == '1') % 2 != 0


def distance_map(map: List[List[bool]], src: (int, int)) -> List[List[int]]:
    dist = [[sys.maxsize for _ in range(len(map[0]))] for _ in range(len(map))]

    eval = [src]
    dist[src[1]][src[0]] = 0

    while eval:
        cell = eval.pop()
        adj = [(cell[0] + x, cell[1] + y) for (x, y) in [(-1, 0), (0, 1), (1, 0), (0, -1)]]
        adj = [(x, y) for (x, y) in adj if valid_pos(map, x, y) and not map[y][x]]

        newdist = dist[cell[1]][cell[0]] + 1
        for (x, y) in adj:
            if newdist < dist[y][x]:
                dist[y][x] = newdist
                eval.append((x, y))

    return dist


def valid_pos(map, x, y):
    return 0 <= x < len(map[0]) and 0 <= y < len(map)


def map_str(data: List[List[bool]]) -> str:
    return ''.join([''.join([*['#' if v else '.' for v in row], '\n']) for row in data])


class Tests(TestCase):
    def test_generation(self):
        self.assertEqual(".#.####.##\n..#..#...#\n#....##...\n###.#.###.\n.##..#..#.\n..##....#.\n#...##.###\n",
                         map_str(generate(10, 7, 10)))

    def test_pathfinding(self):
        self.assertEqual(11, distance_map(generate(10, 7, 10), (1, 1))[4][7])
