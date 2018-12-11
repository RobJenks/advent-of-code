#pragma once

#include "../base/AOCSolution.h"
#include <array>

class Day2 : public AOCSolution
{
public:

    // Entry point
    void Run(void) const;


private:

    void Part1(void) const;


    std::array<int, 26U> ParseID(const std::string id) const;

};