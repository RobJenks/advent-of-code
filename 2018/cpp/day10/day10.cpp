#include "day10.h"
#include <iostream>
#include <numeric>
#include <algorithm>


void Day10::Run(void) const
{
    std::cout << "\nDay 10:\n";

    RunTests();
    Solve();
}

void Day10::Solve(void) const
{
    std::cout << "\n- Solution -\n";
    Execute("day10/input.txt");
}

void Day10::RunTests(void) const
{
    std::cout << "\n- Tests -\n";
    Execute("day10/tests.txt");
}

void Day10::Execute(const fs::path & file) const
{
    std::vector<std::string> input = GetLines(ReadInput(file));

    std::vector<Point> points;
    std::transform(input.begin(), input.end(), std::back_inserter(points), [this](const auto & el) { return ParseInput(el); });

    int steps = EvaluateToConvergence(points);

    std::cout << "Part 1 result:\n\n";
    std::cout << RenderToString(points) << "\n";

    std::cout << "Part 2 result: " << steps << " timesteps\n";
}

Day10::Point Day10::ParseInput(const std::string & input) const
{
    std::stringstream ss(input);
    char sink[11]; char csink;
    Point point;

    ss.read(&sink[0], 10);
    ss >> point.Position.x >> csink >> point.Position.y >> csink;
    ss.read(&sink[0], 11);    
    ss >> point.Velocity.x >> csink >> point.Velocity.y;

    return point;
}

// Evaluate the problem until points have finished converging to a minimum area
int Day10::EvaluateToConvergence(std::vector<Point> & points) const
{
    int time = 0;
    Vec area = DetermineArea(points);

    while (true)
    {
        ++time;
        EvaluateTimestep(points);
        
        Vec new_area = DetermineArea(points);
        if (new_area >= area)
        {
            ReverseTimestep(points);
            return (time-1);
        }

        area = new_area;
    }

    // Something bad has happened
    return std::numeric_limits<int>::max();
}

// Evaluate a single timestep of the simulation
void Day10::EvaluateTimestep(std::vector<Point> & points) const
{
    std::for_each(points.begin(), points.end(), [](auto & el) { el.Position += el.Velocity; });
}

// Reverse a single timestep of the simulation
void Day10::ReverseTimestep(std::vector<Point> & points) const
{
    std::for_each(points.begin(), points.end(), [](auto & el) { el.Position -= el.Velocity; });
}

// Determine the maximum bounds of the point set
std::tuple<Day10::Vec, Day10::Vec> Day10::DetermineBounds(const std::vector<Point> & points) const
{
    return std::accumulate(points.begin(), points.end(), std::tuple<Vec,Vec>{ Vec(std::numeric_limits<int>::max()), Vec(0) },
        [](const std::tuple<Vec, Vec> & acc, const Point & el) 
            { return std::tuple<Vec, Vec> { Vec::Min(std::get<0>(acc), el.Position), Vec::Max(std::get<1>(acc), el.Position) }; });
}

// Determine the area covered by the point set
Day10::Vec Day10::DetermineArea(const std::vector<Point> & points) const
{
    auto bounds = DetermineBounds(points);
    return (std::get<1>(bounds) - std::get<0>(bounds));
}

// Generate a collection of positions sorted for sequential rendering (positions sorted on Y asc, then on X asc)
std::vector<Day10::Vec> Day10::GetRenderSortedPositions(const std::vector<Point> & points) const
{
    std::vector<Vec> positions;
    positions.reserve(points.size());
    std::transform(points.begin(), points.end(), std::back_inserter(positions), [](const auto & el) { return el.Position; });

    // Sort on y-coordinate first, then on x coordinate within each row
    std::sort(positions.begin(), positions.end(), [](const auto & x0, const auto & x1)
        { return (x0.y < x1.y || (x0.y == x1.y && x0.x < x1.x)); });

    return positions;
}

// Render the given point collection to a string for output
std::string Day10::RenderToString(const std::vector<Point> & points) const
{
    std::stringstream ss;

    auto bounds = DetermineBounds(points);
    Vec offset = std::get<0>(bounds);
    Vec size = std::get<1>(bounds) - std::get<0>(bounds);

    std::vector<Vec> positions = GetRenderSortedPositions(points);
    positions.push_back({ -1 });   // Add terminator to avoid end() checks
    auto nextposition = positions.begin();

    for (int y = 0; y <= size.y; ++y)
    {
        for (int x = 0; x <= size.x; ++x)
        {
            Vec position(x + offset.x, y + offset.y);
            if (position == *nextposition)
            {
                ss << '#';
                while (position == *nextposition) ++nextposition;   // To account for duplicate positions
            }
            else
            {
                ss << '.';
            }
        }
        ss << '\n';
    }
    
    return ss.str();
}