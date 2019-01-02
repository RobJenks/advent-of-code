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


    void Part1(void) const;
    
    std::vector<InstructionScenario> ParseInstructionScenarios(const std::vector<std::string> & input) const;

    bool ScenarioMatchesOpcode(const InstructionScenario & scenario, int opcode) const;
    int GetPossibleOpcodeMatches(const InstructionScenario & scenario) const;
    bool ScenarioMatchesAtLeastThreeOpCodes(const InstructionScenario & scenario) const;

};