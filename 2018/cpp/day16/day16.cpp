#include "day16.h"
#include <iostream>
#include <sstream>
#include <numeric>
#include "../common/Test.h"
#include "../common/StringUtil.h"
#include "Opcode.h"
#include "CPU.h"

void Day16::Run(void) const
{
    std::cout << "\nDay 16:\n";

    RunTests();
    Part1();
    Part2();
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

    Test::AssertVerbose(GetPossibleOpcodeMatches(scenario).size(), 3Ui64, "Scenario does not match expected number of opcodes", "Opcode scenario count");
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

    std::cout << "\nPart 1 result: " << result << "\n\n";
}

void Day16::Part2(void) const
{
    std::vector<std::string> input = GetLines(ReadInput("day16/input.txt"));
    auto scenarios = ParseInstructionScenarios(input);

    auto program = ParseProgramInput(input.begin() + 3222, input.end());
    auto mapping = DetermineOpcodeMapping(scenarios);

    CPU _cpu; Registers reg;
    for (const auto & instr : program)
    {
        _cpu.EvaluateInPlaceAs(instr, mapping[instr.OpCode()], reg);
    }

    std::cout << "\nPart 2 result: " << reg[0] << " (final register state: " << reg << ")\n\n";
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

std::vector<Instruction> Day16::ParseProgramInput(const std::vector<std::string>::const_iterator start, 
                                                  const std::vector<std::string>::const_iterator end) const
{
    std::vector<Instruction> instructions;

    for (auto it = start; it != end; ++it)
    {
        std::string line = StringUtil::Trim(*it);
        if (line.empty()) continue;

        Instruction instr;
        std::stringstream ss(line);
        ss >> instr.val[0] >> instr.val[1] >> instr.val[2] >> instr.val[3];

        instructions.push_back(instr);
    }

    return instructions;
}

bool Day16::ScenarioMatchesOpcode(const InstructionScenario & scenario, int opcode) const
{
    CPU _cpu;

    if (_cpu.EvaluateAs(scenario.instruction, opcode, scenario.before) != scenario.after) return false;

    return (!_cpu.HasErrors());
}

std::vector<int> Day16::GetPossibleOpcodeMatches(const InstructionScenario & scenario) const
{
    std::vector<int> ops;
    for (int i = 0; i < static_cast<int>(Opcode::_COUNT); ++i)
    {
        if (ScenarioMatchesOpcode(scenario, i)) ops.push_back(i);
    }

    return ops;
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

Day16::OpcodeMapping Day16::DetermineOpcodeMapping(const std::vector<InstructionScenario> & scenarios) const
{
    const int OPCOUNT = static_cast<int>(Opcode::_COUNT);
    
    // Generate array showing each value and which opcodes may possibly match it
    std::array<std::array<bool, OPCOUNT>, OPCOUNT> potential;
    std::for_each(potential.begin(), potential.end(), [](auto & el) { std::fill(el.begin(), el.end(), true); });

    for (const auto & scenario : scenarios)
    {
        for (int op = 0; op < OPCOUNT; ++op)
        {
            if (!ScenarioMatchesOpcode(scenario, op))
            {
                potential[scenario.instruction.OpCode()][op] = false;
            }
        }
    }
    
    // Now process remaining possibilities to look for definite cases, reducing at each iteration
    std::vector<std::pair<int, int>> confirmed; 
    std::vector<int> remaining;
    for (int i = 0; i < OPCOUNT; ++i) remaining.push_back(i);

    while (!remaining.empty())
    {
        // Look for an opcode with only one possibility
        bool found = false;
        for (const auto op : remaining)
        {
            int poss = std::accumulate(potential[op].cbegin(), potential[op].cend(), 0);
            if (poss == 0)
            {
                std::cout << "ERROR: No remaining possibilities for opcode value " << op << "; fatal error\n";
                assert(false);
            }
            else if (poss == 1)
            {
                int opcode = 0;
                for (opcode = 0; opcode < OPCOUNT; ++opcode) { if (potential[op][opcode] == true) break; }
                assert(opcode != OPCOUNT);

                confirmed.push_back({ op, opcode });
                remaining.erase(std::find(remaining.cbegin(), remaining.cend(), op));

                for (int i = 0; i < OPCOUNT; ++i) if (i != op) potential[i][opcode] = false;

                std::cout << "Confirmed opcode value " << op << " maps to opcode \"" << OpcodeString(opcode) << "\" (" << opcode << ")\n";
                found = true;
                break;
            }
        }

        // Terminate on ambiguous mapping.  This would require branching evaluation to determine correct combination
        if (!found)
        {
            std::cout << "Error: Could not derive any further opcode values; no unambiguous mappings\n";
            std::cout << "Final state:\n";
            for (int i = 0; i < OPCOUNT; ++i)
            {
                std::cout << "Op " << i << ": ";
                std::for_each(potential[i].cbegin(), potential[i].cend(), [](bool el) { std::cout << el << " "; });

                auto it = std::find_if(confirmed.cbegin(), confirmed.cend(), [i](const auto & el) { return (el.first == i); });
                if (it != confirmed.cend()) std::cout << " [" << OpcodeString(it->second) << "]";
                std::cout << "\n";
            }
            assert(false);
        }
    }

    // Compile and return final opcode mapping { Value -> Opcode }
    OpcodeMapping mapping;
    std::for_each(confirmed.cbegin(), confirmed.cend(), [&mapping](const auto & el) {
        mapping[el.first] = el.second;
    });

    return mapping;
}