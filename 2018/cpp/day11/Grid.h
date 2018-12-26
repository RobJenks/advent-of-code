#pragma once

#include <array>
#include <numeric>
#include "../common/Vec2.h"

template <int XSize, int YSize>
class Grid
{
public:

    inline Grid(int serial) 
        : 
        m_serial(serial), 
        m_power{ 0 },
        m_groups{ 0 }, 
        m_groupsize(0)
    {
        Initialise();
    }

    // Returns the power level at the given coordinates
    inline constexpr int Power(int x, int y) const { return m_power[Index(x, y)]; }

    // Returns the coordinate of the highest power group, and its total power level
    std::tuple<Vec2<int>, int> GetHighestPowerGroup(int group_size);

    // Returns the coordinate of the highest power group, its total power level and the size of the
    // group.  Will assess every possible group size in the range [1, max_group]
    std::tuple<Vec2<int>, int, int> GetHighestPowerGroupInRange(int max_group);

private:

    static const int SIZE = (XSize * YSize);
    typedef std::array<int, SIZE> TData;
  
    void Initialise(void);
    void InitialisePower(void);
    void InitialiseGroups(int group_size);

    inline constexpr size_t Index(int x, int y) const { return ((x-1) + ((y-1) * XSize)); }
    inline constexpr Vec2<int> Coord(size_t index) const { return Vec2<int>((static_cast<int>(index) % XSize) + 1, (static_cast<int>(index) / XSize) + 1); }

    constexpr int GroupBorder(const int index, const int group_size) const;

    constexpr int DeterminePowerLevel(int x, int y) const;
    constexpr int DetermineGroupLevel(int x, int y) const;


private:

    int                     m_serial;
    TData                   m_power;
    TData                   m_groups;
    int                     m_groupsize;
};

template <int XSize, int YSize>
constexpr int Grid<XSize, YSize>::DeterminePowerLevel(int x, int y) const
{
    int rack_id = (x + 10);
    int power = ((rack_id * y) + m_serial) * rack_id;

    // Get hundreds digit of the value, or 0 if not available
    if (power < 100)
        power = 0;
    else
        power = ((power / 100) % 10);
    
    power -= 5;
    return power;
}

template <int XSize, int YSize>
constexpr int Grid<XSize, YSize>::GroupBorder(const int index, const int group_size) const
{
    int total = 0;
    const int group_bound = (group_size - 1);
    
    const int bottom_index = (index + (group_bound * XSize));
    for (int i = 0; i < group_size; ++i) total += m_power[bottom_index + i];
    
    const int right_index = (index + group_bound);
    for (int i = 0; i < group_bound; ++i) total += m_power[right_index + (i*XSize)];

    return total;
}

template <int XSize, int YSize>
void Grid<XSize, YSize>::Initialise(void)
{
    InitialisePower();
}

template <int XSize, int YSize>
void Grid<XSize, YSize>::InitialisePower(void)
{
    int x = 1, y = 1;
    for (int i = 0; i < SIZE; ++i)
    {
        m_power[i] = DeterminePowerLevel(x, y);
        if (++x == (XSize + 1)) { x = 1; ++y; }
    }
}

template <int XSize, int YSize>
void Grid<XSize, YSize>::InitialiseGroups(int group_size)
{
    m_groupsize = group_size;

    int x = 1, y = 1;
    Vec2<int> bounds = (Vec2(XSize, YSize) - Vec2(m_groupsize)) + Vec2(1);
    for (int i = 0; i < SIZE; ++i)
    {
        if (x <= bounds.x && y <= bounds.y)
        {
            m_groups[i] = DetermineGroupLevel(x, y);
        }
        else m_groups[i] = 0;

        if (++x == (XSize + 1)) { x = 1; ++y; }
    }
}


// Determines the power level of the group with top-left cell (x,y).  No bounds checking; coords must be valid
template <int XSize, int YSize>
constexpr int Grid<XSize, YSize>::DetermineGroupLevel(int x, int y) const
{
    int total = 0;
    for (int xi = 0; xi < m_groupsize; ++xi)
    {
        for (int yi = 0; yi < m_groupsize; ++yi)
        {
            total += Power(x + xi, y + yi);
        }
    }

    return total;
}

// Returns the coordinate of the highest power group, and its total power level
template <int XSize, int YSize>
std::tuple<Vec2<int>, int> Grid<XSize, YSize>::GetHighestPowerGroup(int group_size)
{
    if (group_size != m_groupsize) InitialiseGroups(group_size);

    int index = 0;
    std::tuple<int, int> result = std::accumulate(m_groups.begin(), m_groups.end(), std::tuple{ -1, -1 }, [&index](const auto & acc, int el) 
    { 
        if (el > std::get<1>(acc)) { return std::tuple<int,int>{ index++, el }; }
        ++index;
        return acc;
    });

    return { Coord(std::get<0>(result)), std::get<1>(result) };
}


// Returns the coordinate of the highest power group, its total power level and the size of the
// group.  Will assess every possible group size in the range [1, max_group]
template <int XSize, int YSize>
std::tuple<Vec2<int>, int, int> Grid<XSize, YSize>::GetHighestPowerGroupInRange(int max_group)
{
    int best_power = 0, best_groupsize = 0;
    Vec2 best_coord = Vec2(-1);

    // Initialise as size-1 groups and iterate from group size of 2 onwards.  Makes a 
    // simplifying assumption that the trivial 1-element groups will not contain the overall maximum
    m_groups = m_power;     

    std::cout << "\nCalculating: ";
    int progress = 0;
    const int progress_interval = ((max_group - 1) / 10);

    for (int g = 2; g <= max_group; ++g)
    {
        if (++progress == progress_interval) { progress = 0;  std::cout << "."; }
        const Vec2 bounds(XSize - g + 1 + 1, YSize - g + 1 + 1);

        int x = 1, y = 1;
        for (int i = 0; i < SIZE; ++i)
        {            
            // Can transform NxN groups to (N+1)x(N+1) groups by adding just the border elements
            m_groups[i] += GroupBorder(i, g);

            if (m_groups[i] > best_power)
            {
                best_power = m_groups[i];
                best_coord = Vec2(x, y);
                best_groupsize = g;
            }

            if (++x == bounds.x)
            {
                if (++y == bounds.y) break; // New row; skip all remaining rows once we can no longer create a full group
                x = 1;                      // Return to start of the new row
                i += (g - 1);                     // Skip forward <groupsize> elements that we don't need to process
            }
        }
    }

    std::cout << "\n";
    return { best_coord, best_power, best_groupsize };
}