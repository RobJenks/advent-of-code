#include "day6.h"
#include <iostream>
#include <sstream>
#include <numeric>


void Day6::Run(void) const
{
    std::cout << "\nDay 6:\n";

    Part1();
}

void Day6::Part1(void) const
{
    std::vector<std::string> input = GetLines(ReadInput(fs::path("day6/input.txt")));
    
    std::vector<Point> points;
    std::transform(input.begin(), input.end(), std::back_inserter(points), [this](const auto & s) { return ParsePoint(s); });

    Point bounds = DetermineBounds(points) + Point(1, 1);
    Grid grid = Grid(bounds);


}

Point Day6::ParsePoint(const std::string & input_string) const
{
    char sink;
    Point point;

    std::stringstream ss(input_string);
    ss >> point.x >> sink >> point.y;

    return point;
}

Point Day6::DetermineBounds(const std::vector<Point> & points) const
{
    return std::accumulate(points.begin(), points.end(), Point(), 
        [](const Point & acc, const Point & el) { return Point(std::max(acc.x, el.x), std::max(acc.y, el.y)); }
    );
}
