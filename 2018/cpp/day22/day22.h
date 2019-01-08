#pragma once

#include "../base/AOCSolution.h"
#include <vector>
#include <tuple>
#include "../common/Vec2.h"


class Day22 : public AOCSolution
{
public:

    // Entry point
    void Run(void) const;


private:

    void RunTests(void) const;
    void Part1(void) const;


private:

    typedef std::pair<int, Vec2<int>> InputData;
    InputData ParseInput(const std::vector<std::string> & input) const;



};