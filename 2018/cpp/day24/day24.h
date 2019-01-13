#pragma once

#include "../base/AOCSolution.h"
#include <vector>
#include "ImmuneCombat.h"


class Day24 : public AOCSolution
{
public:

    // Entry point
    void Run(void) const;


private:

    void RunTests(void) const;
    void TestParsing(void) const;
    void TestCombat(void) const;
    void TestCombatPrecedence(void) const;
    void TestSpecialStats(void) const;
    
    void Part1(void) const;


private:

    ImmuneCombat ParseInput(const std::vector<std::string> & input) const;
    std::pair<std::vector<Damage::Type>, std::vector<Damage::Type>> ParseSpecialProperties(const std::string & input) const;

};