#pragma once

#include "../base/AOCSolution.h"
#include "Vec2.h"
#include <string>

class Day10 : public AOCSolution
{
public:

    // Entry point
    void Run(void) const;


private:

    typedef Vec2<int> Vec;

    struct Point
    {
        Vec Position;
        Vec Velocity;

        Point(void) : Position(0), Velocity(0) { }
        Point(const Vec & position, const Vec & velocity) : Position(position), Velocity(velocity) { }
        Point(Vec && position, Vec && velocity) : Position(std::move(position)), Velocity(std::move(velocity)) { }
    };

private:

    void RunTests(void) const; 
    void Solve(void) const;
    
    Point ParseInput(const std::string & input) const;

    void Execute(const fs::path & file) const;

    // Evaluate the problem until points have finished converging to a minimum area
    int EvaluateToConvergence(std::vector<Point> & points) const;

    // Evaluate or reverse a single timestep of the simulation
    void EvaluateTimestep(std::vector<Point> & points) const;
    void ReverseTimestep(std::vector<Point> & points) const;

    // Determine the area covered by the point set
    std::tuple<Vec, Vec> DetermineBounds(const std::vector<Point> & points) const;
    Vec DetermineArea(const std::vector<Point> & points) const;

    // Generate a collection of positions sorted for sequential rendering (positions sorted on Y asc, then on X asc)
    std::vector<Vec> GetRenderSortedPositions(const std::vector<Point> & points) const;

    // Render the given point collection to a string for output
    std::string RenderToString(const std::vector<Point> & points) const;
};