#pragma once

#include "../base/AOCSolution.h"
#include "../day16/Instruction.h"
#include "../day16/Registers.h"


class Day19 : public AOCSolution
{
public:

    // Entry point
    void Run(void) const;


private:

    void RunTests(void) const; 

    void Part1(void) const;
    void Part2(void) const;



private:

    std::vector<Instruction> ParseProgram(const std::vector<std::string> & input) const;

    Registers R6(int x0, int x1, int x2, int x3, int x4, int x5) const { return Registers(6, { x0,x1,x2,x3,x4,x5 }); }

};