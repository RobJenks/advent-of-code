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
    static const char PATTERN_START = '@';
    static const char PATTERN_TERM = '~';

    void Part1(void) const;
    
    std::string Reduce(std::string data) const;

    std::string Collapse(const std::string & data) const;
};