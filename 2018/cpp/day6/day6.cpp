#include "day6.h"
#include <iostream>
#include <sstream>
#include <numeric>
#include <assert.h>


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

    FillSpace(grid, points);
    
    auto sizes = GetAreaSizes(grid, points);
    auto infinite_areas = GetInfiniteAreas(grid);

    int max_area = 0; int area_id = 0;
    for (int i = 1; i < sizes.size(); ++i)
    {
        if (sizes[i] > max_area && infinite_areas.find(i) == infinite_areas.end())
        {
            max_area = sizes[i];
            area_id = i;
        }
    }

    std::cout << "Part 1 result = " << max_area << " (Area " << area_id << ")\n";
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

void Day6::FillSpace(Grid & grid, const std::vector<Point> & points) const
{
    for (int i = 0; i < points.size(); ++i)
    {
        GridPoint & point = grid.Get(points[i].x, points[i].y);
        point.ID = (i + 1);
        point.Distance = 0;

        // Extend this point's influence in coencentric squares out from the centre, stopping when no further influence is spread
        SpreadInfluence(grid, point);
    }
}

void Day6::SpreadInfluence(Grid & grid, const GridPoint & point) const
{
    int id = point.ID;
    Point centre = point.Location;

    std::vector<size_t> cells;
    int d = 1;

    while (true)
    {
        cells.clear();
        bool spread = false;

        Point pmin = centre - Point(d, d);
        Point pmax = centre + Point(d, d);

        // Add top/bottom cells, then left/right cells, to collection for testing
        for (int i = pmin.x; i <= pmax.x; ++i)
        {
            if (grid.ValidIndex(i, pmin.y)) cells.push_back(grid.Index(i, pmin.y ));
            if (grid.ValidIndex(i, pmax.y)) cells.push_back(grid.Index(i, pmax.y ));
        }
        for (int i = pmin.y + 1; i <= pmax.y - 1; ++i)
        {
            if (grid.ValidIndex(pmin.x, i)) cells.push_back(grid.Index(pmin.x, i ));
            if (grid.ValidIndex(pmax.x, i)) cells.push_back(grid.Index(pmax.x, i ));
        }

        // Test cells and spread influence if applicable
        for (size_t index : cells)
        {
            GridPoint & cell = grid.Points[index];
            assert(cell.ID != id);
            int dist = centre.ManhattanDistance(cell.Location);

            if (cell.ID == GridPoint::AREA_NONE ||
                cell.Distance > dist)
            {
                // Either an unclaimed cell, or a claimed cell that we are closer to and so can take over
                cell.ID = id;
                cell.Distance = dist;
                spread = true;
            }
            else
            {
                if (cell.Distance == dist)
                {
                    // Contested cell; no overall owner
                    cell.ID = GridPoint::AREA_SHARED;
                    spread = true;
                }
                else
                {
                    // Claimed by another, closer point
                    continue;
                }
            }
        }

        // Stop expanding influence if we failed to spread to any cells this cycle.  Otherwise, expand outwards
        if (!spread) break;
        ++d;
    }
}

// Returns a 1-based collection mapping area IDs to the size of their influence area
std::vector<int> Day6::GetAreaSizes(const Grid & grid, const std::vector<Point> points) const
{
    std::vector<int> sizes;
    sizes.insert(sizes.begin(), points.size() + 1, 0);

    std::for_each(grid.Points.begin(), grid.Points.end(), [&sizes](const GridPoint & el) { ++sizes[el.ID]; });

    return sizes;
}

// Approximate set of infinite areas as those which extend to the grid boundaries.  Not guaranteed, but good enough here
std::unordered_set<int> Day6::GetInfiniteAreas(const Grid & grid) const
{
    std::unordered_set<int> inf;

    for (int i = 0; i < grid.Size.x; ++i)
    {
        inf.emplace(grid.Get(i, 0).ID);
        inf.emplace(grid.Get(i, grid.Size.y - 1).ID);
    }
    for (int i = 1; i < grid.Size.y - 1; ++i)
    {
        inf.emplace(grid.Get(0, i).ID);
        inf.emplace(grid.Get(grid.Size.x - 1, i).ID);
    }

    return inf;
}
