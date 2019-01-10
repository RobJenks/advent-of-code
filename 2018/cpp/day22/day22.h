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
    void TestRiskAssessment(void) const;
    void TestTraversalCost(void) const;

    void Solve(void) const;


private:

    typedef std::pair<int, Vec2<int>> InputData;
    InputData ParseInput(const std::vector<std::string> & input) const;



};