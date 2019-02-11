from unittest import TestCase
from typing import List
from enum import IntEnum
from common import io

def run():
    print("Part 1 result:", part1())
    print("Part 2 result:", part2())


def part1():
    inst = parse_input("day12/input.txt")
    (reg_final, _) = execute(inst, [0, 0, 0, 0])

    return reg_final[0]


def part2():
    inst = parse_input("day12/input.txt")
    (reg_final, _) = execute(inst, [0, 0, 1, 0])

    return reg_final[0]


def parse_input(path):
    return [Instruction(INST_ID[x[0]], [int(y) if is_numeric(y) else y for y in x[1:]]) for x in
            [line.split() for line in io.read_file(path).splitlines(False)]]


def execute(instructions, registers: List[int]) -> (List[int], int):    # Returns (Registers, PC)
    pc = 0
    while pc < len(instructions):
        inst = instructions[pc]
        if inst.typ == Inst.cpy:
            registers[register_id(inst.args[1])] = \
                registers[register_id(inst.args[0])] if isinstance(inst.args[0], str) else inst.args[0]

        elif inst.typ == Inst.inc:
            registers[register_id(inst.args[0])] += 1

        elif inst.typ == Inst.dec:
            registers[register_id(inst.args[0])] -= 1

        elif inst.typ == Inst.jnz:
            if 0 != (registers[register_id(inst.args[0])] if isinstance(inst.args[0], str) else inst.args[0]):
                pc += (inst.args[1] - 1)    # -1 since we will increment the PC immediately after each instruction

        pc += 1

    return (registers, pc)


def register_id(reg: str) -> int:
    return ord(reg) - ord('a')


def is_numeric(x) -> bool:
    return x.isnumeric() or (x[1:].isnumeric() and x[0:1] == '-')


class Inst(IntEnum):
    cpy = 0
    inc = 1
    dec = 2
    jnz = 3


class Instruction:
    typ: Inst
    args: List

    def __init__(self, _typ, _args):
        self.typ = _typ
        self.args = _args

    def __str__(self):
        return "{}{}".format(INST_NAMES[self.typ], str(self.args))

    def __repr__(self):
        return self.__str__()


INST_NAMES = {Inst.cpy: "cpy", Inst.inc: "inc", Inst.dec: "dec", Inst.jnz: "jnz"}
INST_ID = dict([(y, x) for (x, y) in INST_NAMES.items()])


class Tests(TestCase):
    def test_program(self):
        inst = parse_input("tests.txt")
        (reg_final, _) = list(execute(inst, [0, 0, 0, 0]))[-1]
        self.assertEqual(42, reg_final[0])