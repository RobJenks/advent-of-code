from unittest import TestCase
from typing import List
from enum import IntEnum
from common import io

def run():
    print("Part 1 result:", part1("day12/input.txt"))
    print("Part 2 result:", part2("day12/input.txt"))


def part1(input):
    (reg_final, _, _) = execute(parse_input(input), [0, 0, 0, 0])

    return reg_final[0]


def part2(input):
    (reg_final, _, _) = execute(parse_input(input), [0, 0, 1, 0])

    return reg_final[0]


def parse_input(path):
    return [Instruction(INST_ID[x[0]], [int(y) if is_numeric(y) else y for y in x[1:]]) for x in
            [line.split() for line in io.read_file(path).splitlines(False)]]


def execute(instructions, registers: List[int]) -> (List[int], int, List[int]):    # Returns (Registers, PC, Output)
    return list(execute_to_output(instructions, registers))[-1]


def execute_to_output(instructions, registers: List[int]) -> (List[int], int, List[int]):    # Returns (Registers, PC, Output)
    pc, n = 0, len(instructions)
    out = []

    while pc < n:
        inst = instructions[pc]
        if inst.typ == Inst.cpy:
            if is_numeric(inst.args[1]): continue
            registers[register_id(inst.args[1])] = resolve_arg(registers, inst.args[0])

        elif inst.typ == Inst.inc:
            if is_numeric(inst.args[0]): continue
            registers[register_id(inst.args[0])] += 1

        elif inst.typ == Inst.dec:
            if is_numeric(inst.args[0]): continue
            registers[register_id(inst.args[0])] -= 1

        elif inst.typ == Inst.jnz:
            if resolve_arg(registers, inst.args[0]) != 0:
                pc += (resolve_arg(registers, inst.args[1]) - 1)    # -1 since we will increment the PC
                                                                    # immediately after each instruction
        elif inst.typ == Inst.tgl:
            tgt = pc + resolve_arg(registers, inst.args[0])
            if 0 <= tgt < n:
                instructions[tgt] = toggle(instructions[tgt])

        elif inst.typ == Inst.mul:
            registers[register_id(inst.args[0])] *= registers[register_id(inst.args[1])]

        elif inst.typ == Inst.out:
            out.append(resolve_arg(registers, inst.args[0]))
            yield registers[:], pc, out[:]

        pc += 1

    yield (registers, pc, out)


def register_id(reg: str) -> int:
    return ord(reg) - ord('a')


def resolve_arg(reg, arg):
    return reg[register_id(arg)] if isinstance(arg, str) else arg


def is_numeric(x) -> bool:
    return x.isnumeric() or (x[1:].isnumeric() and x[0:1] == '-')


def toggle(inst):
    if len(inst.args) == 1:
        return Instruction(Inst.dec if inst.typ == Inst.inc else Inst.inc, inst.args)
    elif len(inst.args) == 2:
        return Instruction(Inst.cpy if inst.typ == Inst.jnz else Inst.jnz, inst.args)
    else:
        raise RuntimeError("Cannot toggle unknown instruction type")


class Inst(IntEnum):
    cpy = 0
    inc = 1
    dec = 2
    jnz = 3
    tgl = 4     # Day 23 extension
    mul = 5     # Day 23 optimiser extension
    out = 6     # Day 24 extension


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


INST_NAMES = {Inst.cpy: "cpy", Inst.inc: "inc", Inst.dec: "dec", Inst.jnz: "jnz",
              Inst.tgl: "tgl", Inst.mul: "mul", Inst.out: "out"}
INST_ID = dict([(y, x) for (x, y) in INST_NAMES.items()])


class Tests(TestCase):
    def test_program(self):
        inst = parse_input("tests.txt")
        (reg_final, _, _) = execute(inst, [0, 0, 0, 0])
        self.assertEqual(42, reg_final[0])

    def test_regression(self):
        self.assertEqual(318007, part1("input.txt"))
        self.assertEqual(9227661, part2("input.txt"))
