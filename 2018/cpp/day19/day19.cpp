#include "day19.h"
#include <iostream>
#include "../common/StringUtil.h"
#include "../common/Test.h"
#include "../day16/CPU.h"
#include "OpcodeMap.h"


void Day19::Run(void) const
{
    std::cout << "\nDay 19:\n";

    RunTests();
    Part1();
}

void Day19::RunTests(void) const
{
    std::vector<std::string> input = GetLines(ReadInput("day19/tests.txt"));
    auto instructions = ParseProgram(input);

    CPU _cpu(true, true);
    Registers registers(6);

    std::cout << "Evaluating test program:\n";
    registers = _cpu.EvaluateProgram(registers, instructions);

    Test::AssertVerbose(registers, R6(6, 5, 6, 0, 0, 9), "Final register state is not correct", "Final register state");
}

void Day19::Part1(void) const
{
    std::vector<std::string> input = GetLines(ReadInput("day19/input.txt"));
    auto instructions = ParseProgram(input);

    CPU _cpu(true, false);
    Registers registers(6);

    registers = _cpu.EvaluateProgram(registers, instructions);

    std::cout << "Part 1 result: " << registers[0] << " (final register state: " << registers << ")\n";
}



std::vector<Instruction> Day19::ParseProgram(const std::vector<std::string> & input) const
{
    OpcodeMap opcodes;
    std::vector<Instruction> instructions;

    for (const auto & in : input)
    {
        auto line = StringUtil::Trim(in);
        if (line.empty()) continue;

        Instruction instr; 
        std::string opcode;;
        std::stringstream ss(line);

        ss >> opcode;
        instr.val[0] = static_cast<int>(opcodes.Get(opcode));

        ss >> instr.val[1] >> instr.val[2] >> instr.val[3];
        instructions.push_back(instr);
    }

    return instructions;
}