#include "day21.h"
#include <iostream>
#include <cassert>
#include "../common/StringUtil.h"
#include "../common/Test.h"
#include "../day16/Instruction.h"
#include "../day16/CPU.h"
#include "../day19/ProgramParser.h"


void Day21::Run(void) const
{
    std::cout << "\nDay 21:\n";

    Part1();
}


void Day21::Part1(void) const
{
    std::vector<std::string> input = GetLines(ReadInput("day21/input.txt"));
    auto instructions = ProgramParser::Parse(input);

    std::cout << "\nPart 1 executing...\n";
    CPU _cpu(true, false);

    // Instruction 28 is testing r0 against r1, and execution will terminate if they are equal
    // 28: eqrr 1 0 5		r5 = (r0 == r1)
    // 29: addr 5 4 4		IP += r5		// I.e. skip to end, if r5 != 0 (if r0 == r1)
    // 30: seti 5 9 4		IP = 5			// Return to start if r5 == 0 (if r0 != r1)

    // Evaluate the program to a halt on instruction 28, then return the value of register r1 as the result
    auto[registers, cycles] = _cpu.EvaluateProgram(Registers(6), instructions, CPUConfig().WithInstructionHalt(28)); 

    std::cout << "Part 1 result = " << registers[1] << " (after " << cycles << " cycles; register state = " << registers << ")\n";
}
