#pragma once

#include "../base/AOCSolution.h"

class Day11 : public AOCSolution
{
public:

    // Entry point
    void Run(void) const;


private:

    static const int PROBLEM_INPUT = 6042;

    void RunTests(void) const; 
    void RunPowerTests(void) const;
    void RunGroupTests(void) const;

    void Part1(void) const;
    
};