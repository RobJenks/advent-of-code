#pragma once

#include "../base/AOCSolution.h"
#include "State.h"
#include "Rule.h"


class Day12 : public AOCSolution
{
public:

    // Entry point
    void Run(void) const;


private:

    void RunTests(void) const; 
    void Part1(void) const;
  
    std::pair<State, std::vector<Rule>> ParseInput(const std::vector<std::string> & input) const;

    void RunSimulationVerbose(State & state, const std::vector<Rule> & rules, unsigned int iterations) const;
};