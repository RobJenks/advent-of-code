from enum import IntEnum
from functools import reduce
from common import io

def run():
    print("Part 1 result:", part1())
    print("Part 2 result:", part2())


def part1():
    return [x for x in follow_instructions(parse_input("day10/input.txt"))
        if x[1] and x[1][0][0] == 17 and x[1][1][0] == 61][0][0]


def part2():
    outputs = ([x for x in follow_instructions(parse_input("day10/input.txt"))][-1])[2]
    return reduce(lambda acc, x: (acc * x[0]), outputs[:3], 1)


def follow_instructions(instr):
    # Perform all one-time initialisation before the generator cycle
    bot_count = list(max([[x[i] for i in [2, 4] if x[i+1] == DestType.Bot] for x in instr if x[0] == InstructionType.Give]))[0] + 1
    out_count = list(max([[x[i] for i in [2, 4] if x[i+1] == DestType.Output] for x in instr if x[0] == InstructionType.Give]))[0] + 1

    bots = [[] for _ in range(bot_count)]
    outputs = [[] for _ in range(out_count)]
    dist = {x[1]: ((x[2], x[3]), (x[4], x[5])) for x in instr if x[0] == InstructionType.Give}

    for x in instr:
        if x[0] == InstructionType.Init:
            bots[x[1]].append(x[2])

    # Evaluation cycle
    eval = [i for (i, x) in enumerate(bots) if len(x) == 2]
    while eval:
        i = eval.pop()
        vals = sorted(bots[i])
        assign = dist[i]

        yield((i, [(vals[ix], assign[ix]) for ix in range(2)], outputs))  # Yield { bot, [(val0, tgt0), (val1, tgt1)], output }

        bots[i] = []
        for (i, x) in enumerate(assign):
            [bots, outputs][x[1] == DestType.Output][x[0]].append(vals[i])

        eval.extend([x[0] for x in assign if x[1] == DestType.Bot and len(bots[x[0]]) == 2])

    yield((None, None, outputs))


def parse_input(path):
    rows = [x.split() for x in io.read_file(path).splitlines(False)]
    return [
        (InstructionType.Init, int(x[5]), int(x[1])) if x[2] == 'goes'      # { bot, init-value }
        else (InstructionType.Give, int(x[1]),                              # { bot, give-low, type-low, give-high, type-high }
              int(x[6]), DestType.Output if x[5] == 'output' else DestType.Bot,
              int(x[11]), DestType.Output if x[10] == 'output' else DestType.Bot) if x[2] == 'gives'

        else "ERROR"
    for x in rows]


class InstructionType(IntEnum):
    Init = 0
    Give = 1

class DestType(IntEnum):
    Bot = 0
    Output = 1

