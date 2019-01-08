#pragma once

#include <vector>
#include <array>
#include "../common/Vec2.h"
#include "Region.h"


class Cave
{
public:

    typedef std::vector<Region> RegionData;
    static const long CALC_MOD = 20183L;

    Cave(int depth, Vec2<int> target);

    inline int                  DetermineErosion(long geo_index) const { return ((geo_index + m_depth) % CALC_MOD); }
    inline Region::RegionType   DetermineRegionType(int erosion) const { return REGION_EROSION[erosion % 3]; }
    Region                      CalculateRegion(long geo_index) const;

    int                         CalculateRiskScore(void) const;

    inline size_t       Index(int x, int y) const { return (x + (y * m_size.x)); }
    inline size_t       Index(const Vec2<int> & v) const { return Index(v.x, v.y); }
    inline Vec2<int>    Coord(size_t index) const { return Vec2(static_cast<int>(index % m_size.x), static_cast<int>(index / m_size.x)); }

    static const size_t NO_CELL = static_cast<size_t>(0U) - static_cast<size_t>(1U);
    size_t              GetLeft(size_t index) const;
    size_t              GetRight(size_t index) const;
    size_t              GetUp(size_t index) const;
    size_t              GetDown(size_t index) const;

    std::string         str(void) const;

private:

    RegionData m_data;
    Vec2<int> m_size;
    size_t m_count;

    int m_depth;
    Vec2<int> m_target;

    static const std::array<Region::RegionType, 3U> REGION_EROSION;
    static const std::array<char, 3U> REGION_SCHEMATIC;
};