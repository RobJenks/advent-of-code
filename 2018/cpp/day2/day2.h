#pragma once

#include "../base/AOCSolution.h"
#include <array>

class Day2 : public AOCSolution
{
public:

    // Entry point
    void Run(void) const;


private:

    static const char MASK = '*';

    void Part1(void) const;
    void Part2(void) const;

    std::array<int, 26U> ParseID(const std::string id) const;
    std::vector<std::string> GenerateMaskedIds(const std::string & id) const;
    std::string PerformMaskedSearch(const std::vector<std::string> & input) const;
};