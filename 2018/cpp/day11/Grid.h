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
        m_groups{ 0 }
    {
        Initialise();
    }

    inline constexpr int Power(int x, int y) const { return m_power[Index(x, y)]; }

    std::tuple<Vec2<int>, int> GetHighestPowerGroup(void) const;

private:

    static const int SIZE = (XSize * YSize);
    typedef std::array<int, SIZE> TData;

    static const int GROUP_SIZE = 3;
  
    void Initialise(void);
    void InitialisePower(void);

    template <int GroupSize>
    void InitialiseGroups(void);

    inline constexpr size_t Index(int x, int y) const { return ((x-1) + ((y-1) * XSize)); }
    inline constexpr Vec2<int> Coord(size_t index) const { return Vec2<int>((static_cast<int>(index) % XSize) + 1, (static_cast<int>(index) / XSize) + 1); }

    constexpr int DeterminePowerLevel(int x, int y) const;

    template <int GroupSize>
    constexpr int DetermineGroupLevel(int x, int y) const;


private:

    int                     m_serial;
    TData                   m_power;
    TData                   m_groups;

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
void Grid<XSize, YSize>::Initialise(void)
{
    InitialisePower();
    InitialiseGroups<GROUP_SIZE>();
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
template <int GroupSize>
void Grid<XSize, YSize>::InitialiseGroups(void)
{
    int x = 1, y = 1;
    Vec2<int> bounds = (Vec2(XSize, YSize) - Vec2(GroupSize, GroupSize)) + Vec2(1);
    for (int i = 0; i < SIZE; ++i)
    {
        if (x <= bounds.x && y <= bounds.y)
        {
            m_groups[i] = DetermineGroupLevel<GROUP_SIZE>(x, y);
        }

        if (++x == (XSize + 1)) { x = 1; ++y; }
    }
}

// Determines the power level of the group with top-left cell (x,y).  No bounds checking; coords must be valid
template <int XSize, int YSize>
template <int GroupSize>
constexpr int Grid<XSize, YSize>::DetermineGroupLevel(int x, int y) const
{
    int total = 0;
    for (int xi = 0; xi < GroupSize; ++xi)
    {
        for (int yi = 0; yi < GroupSize; ++yi)
        {
            total += Power(x + xi, y + yi);
        }
    }

    return total;
}

// Returns the coordinate of the highest power group, and its total power level
template <int XSize, int YSize>
std::tuple<Vec2<int>, int> Grid<XSize, YSize>::GetHighestPowerGroup(void) const
{
    int index = 0;
    std::tuple<int, int> result = std::accumulate(m_groups.begin(), m_groups.end(), std::tuple{ -1, -1 }, [&index](const auto & acc, int el) 
    { 
        if (el > std::get<1>(acc)) { return std::tuple<int,int>{ index++, el }; }
        ++index;
        return acc;
    });

    return { Coord(std::get<0>(result)), std::get<1>(result) };
}