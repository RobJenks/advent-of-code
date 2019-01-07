#pragma once

#include "../base/AOCSolution.h"


class Day20 : public AOCSolution
{
public:

    // Entry point
    void Run(void) const;


private:

    void RunTests(void) const; 

    void Part1(void) const;



private:

    std::string ParseInput(std::string input) const;

};