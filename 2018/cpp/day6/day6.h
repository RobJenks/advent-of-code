#pragma once

#include "../base/AOCSolution.h"
#include <string>
#include "Point.h"
#include "Grid.h"

class Day6 : public AOCSolution
{
public:

    // Entry point
    void Run(void) const;


private:

    void Part1(void) const;

    
    

    Point ParsePoint(const std::string & input_string) const;
    Point DetermineBounds(const std::vector<Point> & points) const;
};