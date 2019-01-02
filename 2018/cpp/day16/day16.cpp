#include "day16.h"
#include <iostream>
#include <sstream>
#include <numeric>
#include "../common/Test.h"
#include "Opcode.h"
#include "CPU.h"

void Day16::Run(void) const
{
    std::cout << "\nDay 16:\n";

    RunTests();
    Part1();
}

void Day16::RunTests(void) const
{
    TestOpcodes();
    TestOpcodeScenario();
}

void Day16::TestOpcodes(void) const
{
    TestInstruction({ 1,2,3,4 }, { Opcode::addr, 1,2,0 }, { 5,2,3,4 });
    TestInstruction({ 1,2,3,4 }, { Opcode::addi, 2,1,0 }, { 4,2,3,4 });
    TestInstruction({ 1,2,3,4 }, { Opcode::mulr, 2,3,1 }, { 1,12,3,4 });
    TestInstruction({ 1,2,3,4 }, { Opcode::muli, 2,3,1 }, { 1,9,3,4 });
    TestInstruction({ 1,2,3,4 }, { Opcode::banr, 1,2,3 }, { 1,2,3,2 });
    TestInstruction({ 1,2,3,4 }, { Opcode::bani, 3,7,0 }, { 4,2,3,4 });
    TestInstruction({ 1,2,3,4 }, { Opcode::borr, 2,3,3 }, { 1,2,3,7 });
    TestInstruction({ 1,2,3,4 }, { Opcode::bori, 2,5,3 }, { 1,2,3,7 });
    TestInstruction({ 1,2,3,4 }, { Opcode::setr, 3,0,0 }, { 4,2,3,4 });
    TestInstruction({ 1,2,3,4 }, { Opcode::seti, 3,0,0 }, { 3,2,3,4 });
    TestInstruction({ 1,2,3,4 }, { Opcode::gtir, 3,1,3 }, { 1,2,3,1 });
    TestInstruction({ 1,2,3,4 }, { Opcode::gtir, 2,1,3 }, { 1,2,3,0 });
    TestInstruction({ 1,2,3,4 }, { Opcode::gtri, 1,0,3 }, { 1,2,3,1 });
    TestInstruction({ 1,2,3,4 }, { Opcode::gtri, 1,2,3 }, { 1,2,3,0 });
    TestInstruction({ 1,2,3,4 }, { Opcode::gtrr, 2,1,3 }, { 1,2,3,1 });
    TestInstruction({ 1,2,3,4 }, { Opcode::gtrr, 1,2,3 }, { 1,2,3,0 });
    TestInstruction({ 1,2,3,4 }, { Opcode::eqir, 3,2,3 }, { 1,2,3,1 });
    TestInstruction({ 1,2,3,4 }, { Opcode::eqir, 3,3,3 }, { 1,2,3,0 });
    TestInstruction({ 1,2,3,4 }, { Opcode::eqri, 0,1,3 }, { 1,2,3,1 });
    TestInstruction({ 1,2,3,4 }, { Opcode::eqri, 1,1,3 }, { 1,2,3,0 });
    TestInstruction({ 1,2,3,4 }, { Opcode::eqrr, 2,2,3 }, { 1,2,3,1 });
    TestInstruction({ 1,2,3,4 }, { Opcode::eqrr, 1,2,3 }, { 1,2,3,0 });
}

void Day16::TestOpcodeScenario(void) const
{
    InstructionScenario scenario({ 3,2,1,1 }, { 9,2,1,2 }, { 3,2,2,1 });

    Test::AssertVerbose(ScenarioMatchesOpcode(scenario, static_cast<int>(Opcode::mulr)), true, "Scenario does not match expected opcode", "Opcode scenario test");
    Test::AssertVerbose(ScenarioMatchesOpcode(scenario, static_cast<int>(Opcode::addi)), true, "Scenario does not match expected opcode", "Opcode scenario test");
    Test::AssertVerbose(ScenarioMatchesOpcode(scenario, static_cast<int>(Opcode::seti)), true, "Scenario does not match expected opcode", "Opcode scenario test");

    Test::AssertVerbose(GetPossibleOpcodeMatches(scenario), 3, "Scenario does not match expected number of opcodes", "Opcode scenario count");
}

void Day16::TestInstruction(Registers && initial_state, Instruction && instr, Registers && expected) const
{
    CPU _cpu;
    
    Test::AssertVerbose(_cpu.Evaluate(instr, initial_state), expected, 
        "Evaluation of instruction \"" + OpcodeString(instr.OpCode()) + "\" did not yield expected result", 
        "Instruction test (" + OpcodeString(instr.OpCode()) + ")");

    if (_cpu.HasErrors())
    {
        std::cout << "ERROR: CPU is in error state following evaluation of last instruction " + instr.str_opname() + "\n";
        assert(false);
    }
}

void Day16::Part1(void) const
{
    std::vector<std::string> input = GetLines(ReadInput("day16/input.txt"));
    auto scenarios = ParseInstructionScenarios(input);

    int result = std::accumulate(scenarios.cbegin(), scenarios.cend(), 0, [this](const int acc, const auto & sc) {
        return (acc + (ScenarioMatchesAtLeastThreeOpCodes(sc) ? 1 : 0));
    });

    std::cout << "Part 1 result: " << result << "\n";
}


std::vector<Day16::InstructionScenario> Day16::ParseInstructionScenarios(const std::vector<std::string> & input) const
{
    char csink;
    std::vector<InstructionScenario> scenarios;

    for (auto it = input.cbegin(); it != input.cend(); ++it)
    {
        if ((*it).find_first_of("Before") != std::string::npos)
        {
            // Found a triplet that represents one scenario
            Registers before, after;
            Instruction instr;

            std::stringstream ss((*it).substr(9U, 10U));
            ss >> before.val[0] >> csink >> before.val[1] >> csink >> before.val[2] >> csink >> before.val[3];

            ss = std::stringstream(*(++it));
            ss >> instr.val[0] >> instr.val[1] >> instr.val[2] >> instr.val[3];

            ss = std::stringstream((*(++it)).substr(9U, 10U));
            ss >> after.val[0] >> csink >> after.val[1] >> csink >> after.val[2] >> csink >> after.val[3];

            scenarios.push_back({ std::move(before), std::move(instr), std::move(after) });
        }
    }

    return scenarios;
}

bool Day16::ScenarioMatchesOpcode(const InstructionScenario & scenario, int opcode) const
{
    CPU _cpu;

    if (_cpu.EvaluateAs(scenario.instruction, opcode, scenario.before) != scenario.after) return false;

    return (!_cpu.HasErrors());
}

int Day16::GetPossibleOpcodeMatches(const InstructionScenario & scenario) const
{
    int matches = 0;
    for (int i = 0; i < static_cast<int>(Opcode::_COUNT); ++i)
    {
        if (ScenarioMatchesOpcode(scenario, i)) ++matches;
    }

    return matches;
}

bool Day16::ScenarioMatchesAtLeastThreeOpCodes(const InstructionScenario & scenario) const
{
    int matches = 0;
    for (int i = 0; i < static_cast<int>(Opcode::_COUNT); ++i)
    {
        if (ScenarioMatchesOpcode(scenario, i))
            if (++matches == 3) return true;
    }

    return false;
}