#include "day6.h"
#include <iostream>
#include <sstream>
#include <numeric>
#include <assert.h>


void Day6::Run(void) const
{
    std::cout << "\nDay 6:\n";

    Part1();
    Part2();
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

void Day6::Part2(void) const
{
    std::vector<std::string> input = GetLines(ReadInput(fs::path("day6/input.txt")));

    std::vector<Point> points;
    std::transform(input.begin(), input.end(), std::back_inserter(points), [this](const auto & s) { return ParsePoint(s); });

    Point bounds = DetermineBounds(points) + Point(1, 1);
    Grid grid = Grid(bounds);

    Point nucleus = DetermineNucleus(points);
    long result = ExpandSafeRegion(grid, points, nucleus);

    std::cout << "Part 2 result = " << result << "\n";
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

        // Determine the set of cells that the expansion (at distance d) will cover this cycle
        DetermineExpansion(grid, centre, d, cells);

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

void Day6::DetermineExpansion(const Grid & grid, const Point & centre, int distance, std::vector<size_t> & outIndices) const
{
    Point pmin = centre - Point(distance, distance);
    Point pmax = centre + Point(distance, distance);

    // Add top/bottom cells, then left/right cells, to collection for testing
    for (int i = pmin.x; i <= pmax.x; ++i)
    {
        if (grid.ValidIndex(i, pmin.y)) outIndices.push_back(grid.Index(i, pmin.y));
        if (grid.ValidIndex(i, pmax.y)) outIndices.push_back(grid.Index(i, pmax.y));
    }
    for (int i = pmin.y + 1; i <= pmax.y - 1; ++i)
    {
        if (grid.ValidIndex(pmin.x, i)) outIndices.push_back(grid.Index(pmin.x, i));
        if (grid.ValidIndex(pmax.x, i)) outIndices.push_back(grid.Index(pmax.x, i));
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

// Determines a starting point for the expansion-search to determine our safe region.  We take the centroid of all 
// given points as a very likely (guaranteed?) candidate for one point within the region
Point Day6::DetermineNucleus(const std::vector<Point> & points) const
{
    // Determine centroid and assert that it does in fact lie within the region
    Point nucleus = std::accumulate(points.begin(), points.end(), Point(0, 0),
        [](Point & acc, const Point & el) { return (acc + el); }
    )
        / Point(static_cast<int>(points.size()), static_cast<int>(points.size()));

    assert(IsWithinSafeRegion(nucleus, points));
    
    return nucleus;
}

long Day6::DistanceFromPoints(const Point & point, const std::vector<Point> & points) const
{
    return std::accumulate(points.begin(), points.end(), static_cast<long>(0L), 
        [point](long acc, const Point & el) { return acc + point.ManhattanDistance(el); }
    );
}

bool Day6::IsWithinSafeRegion(const Point & point, const std::vector<Point> & points) const
{
    long dist = 0L;
    for (const auto & pt : points)
    {
        if ((dist += point.ManhattanDistance(pt)) >= SAFE_THRESHOLD) return false;
    }

    return true;
}

// Expand the safe region in all directions from a central nucleus.  Returns the total size of the 
// safe region after all expansion completes
long Day6::ExpandSafeRegion(Grid & grid, const std::vector<Point> & points, Point nucleus) const
{
    std::vector<size_t> cells;
    int d = 1;
    long size = 1L;     // Account for the 'nucleus' cell

    while (true)
    {
        cells.clear();
        bool spread = false;

        // Determine the set of cells that the expansion (at distance d) will cover this cycle
        DetermineExpansion(grid, nucleus, d, cells);

        // Test cells and spread influence if applicable
        for (size_t index : cells)
        {
            GridPoint & cell = grid.Points[index];
            if (IsWithinSafeRegion(cell.Location, points))
            {
                cell.ID = GridPoint::AREA_SAFE;
                ++size;
                spread = true;
            }
        }

        // If we could not spread to any adjacent cells then the region cannot expand any further, so terminate and return the total size
        if (!spread) return size;
        ++d;
    }
}