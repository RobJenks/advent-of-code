#include "day23.h"
#include <iostream>
#include <numeric>
#include "../common/Test.h"
#include "../common/Vec3.h"


void Day23::Run(void) const
{
    std::cout << "\nDay 23:\n";

    RunTests();
    Part1();
    Part2();
}


void Day23::RunTests(void) const
{
    auto input = GetLines(ReadInput("day23/tests.txt"));
    auto bots = ParseInput(input);

    std::cout << "Running tests...\n";
    const auto result = DetermineHighestCoverageCoord_OctreePartitioning(bots);

    {
        TEST_SCOPE_DEFER_ASSERTIONS

        Test::AssertVerbose(result.Min, {12, 12, 12}, "Invalid result min coord", "Min coord");
        Test::AssertVerbose(result.Max, {12, 12, 12}, "Invalid result max coord", "Max coord");
        Test::AssertVerbose(result.Centre, {12, 12, 12}, "Invalid result centre coord", "Centre coord");
        Test::AssertVerbose(result.Size, 1L, "Invalid result size", "Result size");
        Test::AssertVerbose(result.IsUnit(), true, "Result must be a unit area", "Unit state");
        Test::AssertVerbose(result.InRange, 5, "Incorrect result coverage", "Coverage");
        Test::AssertVerbose(result.DistanceToOrigin, 36LL, "Incorrect distance to origin", "Distance to origin");
    }
    
}

void Day23::Part1(void) const
{
    auto input = GetLines(ReadInput("day23/input.txt"));
    auto bots = ParseInput(input);

    auto strongest = bots.GetStrongest();
    auto in_range = bots.GetBotsInRange(strongest);

    std::cout << "Part 1 result: " << in_range << " (of " << bots.Count() << " bots in range of leader " << bots.Get(strongest) << ")\n";
}

void Day23::Part2(void) const
{
    auto input = GetLines(ReadInput("day23/input.txt"));
    auto bots = ParseInput(input);

    const auto result = DetermineHighestCoverageCoord_LinearPartitioning(bots);
    std::cout << "Part 2 result: " << result.DistanceToOrigin << " (Point " << result.Centre << ", " << result.InRange << " bots in range\n";
}

BotArea Day23::DetermineHighestCoverageCoord_LinearPartitioning(const Bots & bots) const
{
    const long LINEAR_SCALE_FACTOR = 10;

    // Use bot locations as a rough bounding box, given that the highest coverage will almost certainly not be outside it
    const auto & data = bots.GetAll();
    auto[pmin, pmax] = std::accumulate(data.cbegin(), data.cend(), std::make_pair<Vec3<long>, Vec3<long>>({ +999999999L }, { -999999999L }), [](auto acc, const Bot & el) {
        return std::make_pair<Vec3<long>, Vec3<long>>(Vec3<long>::Min(acc.first, el.Position), Vec3<long>::Max(acc.second, el.Position));
    });

    // Expand area linearly to account for maximum bounds in all dimensions
    long maxbound = std::max(std::max(pmax.x, pmax.y), pmax.z);
    long scalar = 1L;
    while (scalar < maxbound)
        scalar *= LINEAR_SCALE_FACTOR;

    auto origin_dist = [](const Vec3<long> & coord) { return (std::abs(coord.x) + std::abs(coord.y) + std::abs(coord.z)); };

    Vec3<long> start = (pmin / scalar);
    Vec3<long> end = (pmax / scalar);
    
    int best_count = 0;
    Vec3<long> best_coord(0L);
    long best_origin_dist = std::numeric_limits<long>::max();
    
    std::cout << "Calculating highest coverage: ";
    while (scalar > 1L)
    {
        std::cout << '.';
        scalar /= LINEAR_SCALE_FACTOR;

        best_coord = { 0,0,0 }; 
        best_count = 0;
        best_origin_dist = std::numeric_limits<long>::max();

        // Process all cells in this region; will scale from coarse -> fine in each iteration.  n^3 but for small n
        auto bound = (end + Vec3<long>(1L));
        for (long x = start.x; x < bound.x; ++x)
        {
            for (long y = start.y; y < bound.y; ++y)
            {
                for (long z = start.z; z < bound.z; ++z)
                {
                    Vec3<long> coord(x, y, z);
                    int count = static_cast<int>(std::count_if(data.cbegin(), data.cend(), [&](const Bot & bot) {
                        return (std::abs((bot.Position.x / scalar) - x) + std::abs((bot.Position.y / scalar) - y)
                            + std::abs((bot.Position.z / scalar) - z) <= (bot.Radius / scalar));
                    }));

                    if (count > best_count)
                    {
                        best_count = count;
                        best_coord = coord;
                        best_origin_dist = origin_dist(coord);
                    }
                    else if (count == best_count)
                    {
                        auto dist = origin_dist(coord);
                        if (dist < best_origin_dist)
                        {
                            best_count = count;
                            best_coord = coord;
                            best_origin_dist = dist;
                        }
                    }
                }
            }
        }

        // Linear scale down into the current best region
        start = ((best_coord - Vec3<long>(1L)) * LINEAR_SCALE_FACTOR);
        end = ((best_coord + Vec3<long>(1L)) * LINEAR_SCALE_FACTOR);
    }
    std::cout << '\n';

    // Reuse from octree implementation for convenience
    BotArea result(best_coord, 1L);
    result.InRange = best_count;
    result.DistanceToOrigin = best_origin_dist;

    return result;
}

BotArea Day23::DetermineHighestCoverageCoord_OctreePartitioning(const Bots & bots) const
{
    // Use bot locations as a rough bounding box, given that the highest coverage will almost certainly not be outside it
    const auto & data = bots.GetAll();
    auto[pmin, pmax] = std::accumulate(data.cbegin(), data.cend(), std::make_pair<Vec3<long>, Vec3<long>>({ +999999999L }, { -999999999L }), [](auto acc, const Bot & el) {
        return std::make_pair<Vec3<long>, Vec3<long>>(Vec3<long>::Min(acc.first, el.Position), Vec3<long>::Max(acc.second, el.Position));
    });

    // Expand to power-of-two Octree for simplicity
    auto bounds = (pmax - pmin);
    long maxbound = std::max(std::max(bounds.x, bounds.y), bounds.z);
    long areasize = 1;
    while (areasize < maxbound)
        areasize <<= 1;

    BotArea start(pmin, areasize);
    start.InRange = static_cast<int>(bots.Count());
    assert(bots.GetBotsInRange(start.Min, start.Max) == bots.Count());


    std::vector<BotArea> areas = { start };
    std::vector<BotArea> new_areas;
    while (true)
    {
        int top_set = 0;
        auto best_count = areas[0].InRange;
        auto best_dist = areas[0].DistanceToOrigin;
        int best_new_count = 0;
        for (top_set = 0; top_set < areas.size(); ++top_set)
            if (areas[top_set].InRange != best_count || areas[top_set].DistanceToOrigin != best_dist) break;

        std::cout << "Partitioning down to " << areas.size() << " nodes with max size " << areas[0].Size << " (" << top_set << " have top count " << best_count << " and dist " << best_dist << ")\n";

        // Check termination condition
        if (areas[0].IsUnit()) break;

        // Process each candidate area
        new_areas.clear();
        for (const auto & area : areas)
        {
            if (area.InRange != best_count) break;
            //if (area.Size < 64 && area.DistanceToOrigin != best_dist) break;

            if (area.IsUnit())
            {
                new_areas.push_back(area);
                continue;
            }

            area.Subdivide(new_areas);
            const auto end = new_areas.size();
            for (size_t i = (end - 8U); i < end; ++i)
            {
                new_areas[i].InRange = bots.GetBotsInRange(new_areas[i].Min, new_areas[i].Max);
                best_new_count = std::max(best_new_count, new_areas[i].InRange);
            }
        }

        areas.clear();
        for (const auto & area : new_areas)
            if (area.InRange == best_new_count) areas.insert(std::lower_bound(areas.cbegin(), areas.cend(), area), area);
    }

    return areas[0];
}



Bots Day23::ParseInput(const std::vector<std::string> & input) const
{
    Bots bots;
    Vec3<long> pos; long radius;

    for (const std::string & line : input)
    {
        sscanf_s(line.c_str(), "pos=<%ld,%ld,%ld>, r=%ld", &pos.x, &pos.y, &pos.z, &radius);
        bots.Add(pos, radius);
    }

    return bots;
}