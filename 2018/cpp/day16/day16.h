#pragma once

#include "../base/AOCSolution.h"
#include "Instruction.h"
#include "Registers.h"

class Day16 : public AOCSolution
{
public:

    // Entry point
    void Run(void) const;


private:

    struct InstructionScenario
    {
        Registers before, after;
        Instruction instruction;

        inline InstructionScenario(void) { }
        inline InstructionScenario(Registers && _before, Instruction && _instr, Registers && _after) : before(_before), instruction(_instr), after(_after) { }
    };

private:

    void RunTests(void) const; 
    void TestOpcodes(void) const;
    void TestOpcodeScenario(void) const;
    void TestInstruction(Registers && initial_state, Instruction && instr, Registers && expected) const;

    typedef std::array<int, static_cast<size_t>(Opcode::_COUNT)> OpcodeMapping;
    OpcodeMapping DetermineOpcodeMapping(const std::vector<InstructionScenario> & scenarios) const;

    void Part1(void) const;
    void Part2(void) const;
    
    std::vector<InstructionScenario> ParseInstructionScenarios(const std::vector<std::string> & input) const;
    std::vector<Instruction> ParseProgramInput(const std::vector<std::string>::const_iterator start, const std::vector<std::string>::const_iterator end) const;

    bool ScenarioMatchesOpcode(const InstructionScenario & scenario, int opcode) const;
    std::vector<int> GetPossibleOpcodeMatches(const InstructionScenario & scenario) const;
    bool ScenarioMatchesAtLeastThreeOpCodes(const InstructionScenario & scenario) const;

};