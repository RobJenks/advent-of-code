#pragma once

#include "../base/AOCSolution.h"
#include <string>
#include <unordered_set>
#include "Point.h"
#include "Grid.h"

class Day6 : public AOCSolution
{
public:

    // Entry point
    void Run(void) const;


private:

    void Part1(void) const;
    void Part2(void) const;

    
    Point ParsePoint(const std::string & input_string) const;
    Point DetermineBounds(const std::vector<Point> & points) const;

    void FillSpace(Grid & grid, const std::vector<Point> & points) const;
    void DetermineExpansion(const Grid & grid, const Point & centre, int distance, std::vector<size_t> & outIndices) const;
    void SpreadInfluence(Grid & grid, const GridPoint & point) const;

    std::vector<int> GetAreaSizes(const Grid & grid, const std::vector<Point> points) const;
    std::unordered_set<int> GetInfiniteAreas(const Grid & grid) const;

    Point DetermineNucleus(const std::vector<Point> & points) const;
    long DistanceFromPoints(const Point & point, const std::vector<Point> & points) const;

    static const long SAFE_THRESHOLD = 10000;
    bool IsWithinSafeRegion(const Point & point, const std::vector<Point> & points) const;
    long ExpandSafeRegion(Grid & grid, const std::vector<Point> & points, Point nucleus) const;
};