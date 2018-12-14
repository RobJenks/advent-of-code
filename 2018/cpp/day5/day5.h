#pragma once

#include "../base/AOCSolution.h"
#include <string>

class Day5 : public AOCSolution
{
public:

    // Entry point
    void Run(void) const;


private:

    static const char REACT_DIFF = ('a' - 'A');
    inline bool will_react(char x, char y) const { return (std::abs(x - y) == REACT_DIFF); }

    static const char REMOVED = '#';
    static const char TERMINATOR = '~';

    void Part1(void) const;
    
    bool Reduce(std::string & data) const;

    std::string Collapse(const std::string & data) const;
    static const int COLLAPSE_INTERVAL = 20;
};