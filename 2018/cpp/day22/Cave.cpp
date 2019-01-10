#include "Cave.h"
#include <numeric>
#include <cassert>
#include <stack>
#include <array>
#include <iostream>

const std::array<Region::RegionType, 3U> Cave::REGION_EROSION = { static_cast<Region::RegionType>(0), static_cast<Region::RegionType>(1),static_cast<Region::RegionType>(2) };
const std::array<char, 3U> Cave::REGION_SCHEMATIC = { '.', '=', '|' };
const int Cave::OPPOSITE_DIRECTION[4] = { Cave::SOUTH, Cave::WEST, Cave::NORTH, Cave::EAST };

Cave::Cave(int depth, Vec2<int> target)
    :
    m_depth(depth),
    m_target(target)
{
    // Calculate the region data within a buffer beyond the target position, to allow for pathfinding outside the immediate area
    m_size = m_target + Vec2<int>(20, 20);
    m_count = (m_size.x * m_size.y);
    m_data.insert(m_data.begin(), m_count, Region());

    // Calculate start and end points directly since they are special cases with geo index == 0
    auto tgt_index = Index(m_target);
    m_data[0] = m_data[tgt_index] = CalculateRegion(0L);

    // Build the region data
    long geo = 0L;
    Vec2<int> pos = { 1, 0 };
    for (size_t i = 1U; i < m_count; ++i)
    {
        if (i == tgt_index) continue;
        else if (pos.y == 0) geo = (pos.x * 16807);
        else if (pos.x == 0) geo = (pos.y * 48271);
        else geo = (m_data[i - 1].ErosionLevel * m_data[i - m_size.x].ErosionLevel);    // [x-1][y] * [x][y-1]

        m_data[i].GeologicIndex = geo;
        m_data[i].ErosionLevel = DetermineErosion(geo);
        m_data[i].Type = DetermineRegionType(m_data[i].ErosionLevel);

        if (++pos.x == m_size.x) { pos.x = 0; ++pos.y; }
    }

    InitialiseTransitionCostData();
}


Region Cave::CalculateRegion(long geo_index) const 
{
    Region region;
    region.GeologicIndex = geo_index;
    region.ErosionLevel = DetermineErosion(geo_index);
    region.Type = DetermineRegionType(region.ErosionLevel);

    return region;
}

int Cave::CalculateRiskScore(void) const
{
    // Risk: Rocky = 1, Wet = 2, Narrow = 3 --> same as the underlying int values, so use them
    int score = 0;
    Vec2<int> pos = { 0, 0 };
    for (size_t i = 0U; i < m_count; ++i)
    {
        if (pos.x <= m_target.x && pos.y <= m_target.y) score += static_cast<int>(m_data[i].Type);
        if (++pos.x == m_size.x) { ++pos.y; pos.x = 0; }
    }

    return score;
}

int Cave::GetShortestPath(Vec2<int> target)
{
    if (m_cost.empty()) PerformPathfinding();

    assert(target >= Vec2<int>(0, 0) && target < m_size);
    return m_distance[Index(target) + m_count]; // Add offset since this is the alternate equipment type
}

void Cave::PerformPathfinding(void)
{
    const size_t NODE_OFFSET = m_count;
    const size_t OFFSETS[2] = { 0U, NODE_OFFSET };
    std::array<size_t, 4U> adj;

    // Cost = array[2*data].  Cost[0..data.size-1] = for 1st equipment option.  Cost[data.size..n-1] = for 2nd option
    // Each element stores the cost of moving {North,East,South,West}, to each of the two available options -> 8 conns per node
    m_cost.clear();
    m_cost.insert(m_cost.begin(), m_count * 2U, PathNode());
    
    for (size_t i = 0U; i < m_count; ++i)
    {
        adj = GetAdjacent(i);
        for (size_t d = 0U; d < 4U; ++d) 
        {
            size_t adj_cell = adj[d];
            if (adj_cell == NO_CELL) continue;

            m_cost[i].Cost[d][0]              = CostBetween(m_data[adj_cell].Type, 0, m_data[i].Type, 0);  
            m_cost[i].Cost[d][1]              = CostBetween(m_data[adj_cell].Type, 1, m_data[i].Type, 0);  

            m_cost[i + NODE_OFFSET].Cost[d][0] = CostBetween(m_data[adj_cell].Type, 0, m_data[i].Type, 1);
            m_cost[i + NODE_OFFSET].Cost[d][1] = CostBetween(m_data[adj_cell].Type, 1, m_data[i].Type, 1);
        }
    }

    // Now cost has been determined, perform pathfinding over the 3D (n*n*2) space to find a shortest path
    m_distance.clear();
    m_distance.insert(m_distance.begin(), m_count * 2U, std::numeric_limits<int>::max());

    m_path.clear(); m_path.insert(m_path.begin(), m_count * 2U, -1);    // Predecessor path pointers

    size_t root = 0U + NODE_OFFSET;     // Start at node 0 with equipment 1, so the 'alternate' instance of node 0
    size_t base_cell;

    std::array<size_t, 4U> nodes;
    std::stack<size_t> eval;
    int src_equip;
    eval.push(root);
    m_distance[root] = 0;

    while (!eval.empty())
    {
        const auto cell = eval.top();
        eval.pop();

        if (cell < m_count) { base_cell = cell; src_equip = 0; }
        else                { base_cell = (cell - m_count); src_equip = 1; }

        nodes = { GetNorth(base_cell), GetEast(base_cell), GetSouth(base_cell), GetWest(base_cell) };

        for (int d = 0; d < 4; ++d)
        {
            auto neighbour = nodes[d];              // Always passed the base version, since we will apply an offset later for the alt
            if (neighbour == NO_CELL) continue;
            auto entry_dir = OppositeDirection(d);

            for (int z = 0; z < 2; ++z)     // Z: layer of the 3D node space, i.e. one of the two possible neighbour nodes in this x,y position
            {
                neighbour += OFFSETS[z];    // { index+0, index+offset }

                auto newdist = (m_distance[cell] + m_cost[neighbour].Cost[entry_dir][src_equip]);
                if (newdist < m_distance[neighbour])
                {
                    m_distance[neighbour] = newdist;
                    m_path[neighbour] = static_cast<int>(cell);
                    eval.push(neighbour);
                }
            }
        }
    }
}

void Cave::InitialiseTransitionCostData(void)
{
    const int SWITCH = 1 + 7;
    const int rocky = 0, wet = 1, narrow = 2;
    const int rocky_climbing = 0, rocky_torch = 1;
    const int wet_climbing = 0, wet_neither = 1;
    const int narrow_torch = 0, narrow_neither = 1;

    // Default all costs to zero; only enumerate transitions between different region types
    for (int src_type = 0; src_type < 3; ++src_type)
        for (int dest_type = 0; dest_type < 3; ++dest_type)
            for (int src_equip = 0; src_equip < 2; ++src_equip)
                for (int dest_equip = 0; dest_equip < 2; ++dest_equip)
                    TRANSITION[src_type][src_equip][dest_type][dest_equip] = 1;

    // Transition costs: [source_type][source_equip][dest_type][dest_equip]
    TRANSITION[rocky][rocky_climbing][wet][wet_climbing] = 1;
    TRANSITION[rocky][rocky_climbing][wet][wet_neither] = SWITCH;
    TRANSITION[rocky][rocky_torch][wet][wet_climbing] = SWITCH;
    TRANSITION[rocky][rocky_torch][wet][wet_neither] = SWITCH;
    
    TRANSITION[rocky][rocky_climbing][narrow][narrow_torch] = SWITCH;
    TRANSITION[rocky][rocky_climbing][narrow][narrow_neither] = SWITCH;
    TRANSITION[rocky][rocky_torch][narrow][narrow_torch] = 1;
    TRANSITION[rocky][rocky_torch][narrow][narrow_neither] = SWITCH;
    
    TRANSITION[wet][wet_climbing][rocky][rocky_climbing] = 1;
    TRANSITION[wet][wet_climbing][rocky][rocky_torch] = SWITCH;
    TRANSITION[wet][wet_neither][rocky][rocky_climbing] = SWITCH;
    TRANSITION[wet][wet_neither][rocky][rocky_torch] = SWITCH;
    
    TRANSITION[wet][wet_climbing][narrow][narrow_torch] = SWITCH;
    TRANSITION[wet][wet_climbing][narrow][narrow_neither] = SWITCH;
    TRANSITION[wet][wet_neither][narrow][narrow_torch] = SWITCH;
    TRANSITION[wet][wet_neither][narrow][narrow_neither] = 1;
    
    TRANSITION[narrow][narrow_torch][rocky][rocky_climbing] = SWITCH;
    TRANSITION[narrow][narrow_torch][rocky][rocky_torch] = 1;
    TRANSITION[narrow][narrow_neither][rocky][rocky_climbing] = SWITCH;
    TRANSITION[narrow][narrow_neither][rocky][rocky_torch] = SWITCH;
    
    TRANSITION[narrow][narrow_torch][wet][wet_climbing] = SWITCH;
    TRANSITION[narrow][narrow_torch][wet][wet_neither] = SWITCH;
    TRANSITION[narrow][narrow_neither][wet][wet_climbing] = SWITCH;
    TRANSITION[narrow][narrow_neither][wet][wet_neither] = 1;

    // Same source & destination terrain; only need to specify where equipment changes
    TRANSITION[rocky][rocky_climbing][rocky][rocky_torch] = SWITCH;
    TRANSITION[rocky][rocky_torch][rocky][rocky_climbing] = SWITCH;
    TRANSITION[wet][wet_climbing][wet][wet_neither] = SWITCH;
    TRANSITION[wet][wet_neither][wet][wet_climbing] = SWITCH;
    TRANSITION[narrow][narrow_torch][narrow][narrow_neither] = SWITCH;
    TRANSITION[narrow][narrow_neither][narrow][narrow_torch] = SWITCH;
}

// Returns the cost from source cell type (with equipment [type0, type1]) to enter the dest cell type
int Cave::CostBetween(Region::RegionType source_type, int source_equipment, Region::RegionType dest_type, int dest_equipment) const
{
    return TRANSITION
        [static_cast<int>(source_type)]     // Source cell type
        [source_equipment]                  // Equipment being used at source
        [static_cast<int>(dest_type)]       // Destination cell type
        [dest_equipment];                   // Equipment to be used at destination
}


size_t Cave::GetNorth(size_t index) const
{
    return (index >= m_size.x ? (index - m_size.x) : NO_CELL);
}

size_t Cave::GetEast(size_t index) const
{
    auto right = (index + 1);
    return (right % m_size.x != 0 ? right : NO_CELL);
}

size_t Cave::GetSouth(size_t index) const
{
    auto down = (index + m_size.x);
    return (down < m_count ? down : NO_CELL);
}

size_t Cave::GetWest(size_t index) const
{
    return (index % m_size.x != 0 ? (index - 1) : NO_CELL);
}

std::array<size_t, 4U> Cave::GetAdjacent(size_t index) const
{
    return { GetNorth(index), GetEast(index), GetSouth(index), GetWest(index) };
}

std::string Cave::str(void) const
{
    std::stringstream ss;

    auto tgt_index = Index(m_target);
    ss << 'M';
    int x = 1;

    for (size_t i = 1U; i < m_count; ++i)
    {
        if (i == tgt_index) ss << 'T';
        else ss << REGION_SCHEMATIC[static_cast<int>(m_data[i].Type)];

        if (++x == m_size.x) { ss << '\n'; x = 0; }
    }
    ss << '\n';

    return ss.str();
}

// Renders a visualisation of the cave system, with path plotted to the given target & using 
// the specified equipment at destination
std::string Cave::str_path(Vec2<int> target, int dest_equipment) const
{
    std::string s = str();

    auto tgt = Index(target) + (dest_equipment * m_count);
    auto cell = tgt;
    
    while ((cell % m_count) != 0U)
    {
        auto base_cell = (cell % m_count);
        auto base_pos = Coord(base_cell);
        auto str_index = base_cell + base_pos.y;     // +y to account for one \n per row

        auto pred = m_path[cell];
        bool changing_equip = !((pred < m_count && cell < m_count) || (pred >= m_count && cell >= m_count));

        if (cell != 0U && cell != tgt)
        {
            s[str_index] = (changing_equip ? 178 : 177);
        }

        cell = pred;
    }

    return s;
}





