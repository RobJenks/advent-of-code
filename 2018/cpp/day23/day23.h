#pragma once

#include "../base/AOCSolution.h"
#include <vector>
#include <tuple>
#include "Bots.h"


class Day23 : public AOCSolution
{
public:

    // Entry point
    void Run(void) const;


private:

    void RunTests(void) const;
    
    void Part1(void) const;


private:

    Bots ParseInput(const std::vector<std::string> & input) const;



};