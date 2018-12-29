#pragma once

#include "../base/AOCSolution.h"

class Day14 : public AOCSolution
{
public:

    // Entry point
    void Run(void) const;


private:

    inline static const std::vector<int> INITIAL_STATE = { 3, 7 };
    inline static const size_t TARGET_RECIPES = 652601U;

    void RunTests(void) const; 
    void RunStateTests(void) const;
    void RunSequenceTests(void) const;
    void RunTerminatorTests(void) const;

    void Part1(void) const;
    void Part2(void) const;



};