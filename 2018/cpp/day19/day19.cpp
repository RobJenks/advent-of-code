#include "day19.h"
#include <iostream>
#include "../common/StringUtil.h"
#include "../common/Test.h"
#include "../day16/CPU.h"
#include "OpcodeMap.h"
#include "ProgramParser.h"


void Day19::Run(void) const
{
    std::cout << "\nDay 19:\n";

    RunTests();
    Part1();
    Part2();
}

void Day19::RunTests(void) const
{
    std::vector<std::string> input = GetLines(ReadInput("day19/tests.txt"));
    auto instructions = ProgramParser::Parse(input);

    CPU _cpu(true, true);
    Registers registers(6);

    std::cout << "Evaluating test program:\n";
    auto result = _cpu.EvaluateProgram(registers, instructions);

    Test::AssertVerbose(result.first, R6(6, 5, 6, 0, 0, 9), "Final register state is not correct", "Final register state");
}

void Day19::Part1(void) const
{
    std::vector<std::string> input = GetLines(ReadInput("day19/input.txt"));
    auto instructions = ProgramParser::Parse(input);

    std::cout << "\nPart 1 executing...\n";
    CPU _cpu(true, false);
    auto [registers, cycles] = _cpu.EvaluateProgram(Registers(6), instructions);

    std::cout << "Part 1 result: " << registers[0] << " (final register state: " << registers << ", cycle count: " << cycles << ")\n";
}

void Day19::Part2(void) const
{
    std::vector<std::string> input = GetLines(ReadInput("day19/input.txt"));
    auto instructions = ProgramParser::Parse(input);

    // Shortcut: program is finding all integer factors of a large number and computing the sum.  Algorithm is very long-running,
    // so can run for a period to identify the number being factored and then directly calculate the factor sum here
    std::cout << "\nPart 2 executing...\n";
    CPU _cpu(true, false);
    auto [registers, cycles] = _cpu.EvaluateProgram(R6(1, 0, 0, 0, 0, 0), instructions, CPUConfig().WithCycleLimit(1000U));

    int number = registers[0]; int reg = 0;
    for (int i = 1; i < registers.Count(); ++i) 
        if (registers[i] > number) { number = registers[i]; reg = i; }
    std::cout << "Identified number " << number << " (in register " << reg << ") being factored after " << cycles << " cycles\n";

    // Directly compute the factor sum
    long total = 0L;
    for (long i = 1L; i <= number; ++i)
    {
        if (number % i == 0) total += i;
    }

    std::cout << "Part 2 result: " << total << " (factor sum of " << number << ")\n";
}
