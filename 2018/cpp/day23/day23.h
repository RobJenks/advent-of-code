#pragma once

#include "../base/AOCSolution.h"
#include <vector>
#include <tuple>
#include "Bots.h"
#include "BotArea.h"


class Day23 : public AOCSolution
{
public:

    // Entry point
    void Run(void) const;


private:

    void RunTests(void) const;
    
    void Part1(void) const;
    void Part2(void) const;


private:

    Bots ParseInput(const std::vector<std::string> & input) const;

    BotArea DetermineHighestCoverageCoord_LinearPartitioning(const Bots & bots) const;
    BotArea DetermineHighestCoverageCoord_OctreePartitioning(const Bots & bots) const;

};