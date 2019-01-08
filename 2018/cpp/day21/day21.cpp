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
    Part2();
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

void Day21::Part2(void) const
{
    std::vector<std::string> input = GetLines(ReadInput("day21/input.txt"));
    auto instructions = ProgramParser::Parse(input);

    std::cout << "\nPart 2 executing...\n";
    CPU _cpu(true, false);

    // Values in the target register are periodic, so accumulate all halting values until we encounter a repeat
    // and then choose the last value BEFORE the cycle repeats
    std::vector<int> values;
    std::unordered_set<int> found;

    // Remove IP# directive from the instruction set and apply it via config instead, since this can otherwise
    // affect insruction ordering between runs
    instructions.erase(std::remove_if(instructions.begin(), instructions.end(), [](const Instruction & instr) { 
        return (instr.OpCode() == static_cast<int>(Opcode::DIRECTIVE_IP)); 
    }));

    Registers registers = Registers(6);
    CPUConfig config = CPUConfig()
        .WithInstructionHalt(28)
        .WithInitialIPR(4);

    int total_executions = 0;
    size_t total_cycles = 0U;
    int result = -1;

    std::cout << "Identifying halting values ('.' = x100): ";
    while (true)
    {
        auto[new_reg, cycles] = _cpu.EvaluateProgram(registers, instructions, config);
        ++total_executions;
        total_cycles += cycles;

        if (found.find(new_reg[1]) != found.end()) 
        {
            // Cycle has repeated, so return the last value before this one
            result = values.back();
            break;
        }

        // Otherwise record this value
        if (values.size() % 100 == 0) std::cout << '.';
        values.push_back(new_reg[1]);
        found.insert(new_reg[1]);

        // Restore program state and set our entry point to IP=29, so we can continue execution where we left off
        registers = new_reg;
        config.WithInstructionEntryPoint(29);    // Start immediately after the last termination point
        assert(registers[0] != registers[1]);    // Validate that the halt condition will not be met by this change
    }

    std::cout << "\n\nPart 2 result = " << result << " (after " << total_executions << " program executions and " 
        << total_cycles << " total cycles; registers = " << registers << ")\n";
}
