#pragma once

#include "../base/AOCSolution.h"
#include "Forest.h"

class Day18 : public AOCSolution
{
public:

    // Entry point
    void Run(void) const;


private:

    void RunTests(void) const; 
    void Part1(void) const;

    Forest CreateForest(const std::vector<std::string> & input) const;
  


};