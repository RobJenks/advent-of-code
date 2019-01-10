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

    int                 CalculateRiskScore(void) const;

    int                 GetShortestPath(Vec2<int> target);
    inline int          GetShortestPath(void) { return GetShortestPath(m_target); }
    
    void                InitialiseTransitionCostData(void);
    int                 CostBetween(Region::RegionType source_type, int source_equipment, Region::RegionType dest_type, int dest_equipment) const;

    inline size_t       Index(int x, int y) const { return (x + (y * m_size.x)); }
    inline size_t       Index(const Vec2<int> & v) const { return Index(v.x, v.y); }
    inline Vec2<int>    Coord(size_t index) const { return Vec2(static_cast<int>(index % m_size.x), static_cast<int>(index / m_size.x)); }

    static const size_t NO_CELL = static_cast<size_t>(0U) - static_cast<size_t>(1U);
    size_t              GetNorth(size_t index) const;
    size_t              GetEast(size_t index) const; 
    size_t              GetSouth(size_t index) const;
    size_t              GetWest(size_t index) const;
    std::array<size_t, 4U> GetAdjacent(size_t index) const;

    std::string         str(void) const;
    std::string         str_path(Vec2<int> target, int dest_equipment) const;

private:

    inline int                  DetermineErosion(long geo_index) const { return ((geo_index + m_depth) % CALC_MOD); }
    inline Region::RegionType   DetermineRegionType(int erosion) const { return REGION_EROSION[erosion % 3]; }
    Region                      CalculateRegion(long geo_index) const;

    void                        PerformPathfinding(void);

private:

    RegionData m_data;
    Vec2<int> m_size;
    size_t m_count;

    int m_depth;
    Vec2<int> m_target;

    typedef std::array<int, 2U> PathCost;
    struct PathNode
    {
        std::array<PathCost, 4U> Cost;  // { { Left1, Left2 }, { Up1, Up2 }, ... }
        PathNode(void) { Cost[0] = { 1,1 }; Cost[1] = { 1,1 }; Cost[2] = { 1,1 }; Cost[3] = { 1,1 }; }
    };
    std::vector<PathNode> m_cost;       // Cost of entering the node, using each of the two permissable equipment types
    std::vector<int> m_distance;        // Cost-weighted 'distance' between nodes in the 3D node space
    std::vector<int> m_path;            // Path pointers to predecessor, populated after pathfinding is completed for the full area

    int TRANSITION[3][2][3][2];         // [source_type][source_equip][dest_type][dest_equip]

    static const std::array<Region::RegionType, 3U> REGION_EROSION;
    static const std::array<char, 3U> REGION_SCHEMATIC;

    static const int NORTH = 0;
    static const int EAST = 1;
    static const int SOUTH = 2;
    static const int WEST = 3;

    static const int OPPOSITE_DIRECTION[4];
    static int OppositeDirection(int dir) { return OPPOSITE_DIRECTION[dir]; }
};