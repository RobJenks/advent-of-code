#pragma once

#include "../base/AOCSolution.h"
#include <vector>
#include "../common/Vec4.h"
#include "Points.h"

class Day25 : public AOCSolution
{
public:

    // Entry point
    void Run(void) const;


private:

    void RunTests(void) const;
    
    void Part1(void) const;
    

private:

    Points ParseInput(const std::vector<std::string> & input) const;

};