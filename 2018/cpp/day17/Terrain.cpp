#include "Terrain.h"
#include <iostream>
#include <sstream>
#include <numeric>
#include <algorithm>
#include <cassert>


Terrain::Terrain(const TerrainInputCollection & input, Vec2<int> water_source)
    :
    m_offset(0), 
    m_size(0), 
    m_count(0U),
    m_unoffset_max(0)
{
    // Determine terrain bounds
    TerrainInputDef bounds = std::accumulate(input.cbegin(), input.cend(), MakeTerrainInputDef(10000, -10000, 10000, -10000),
        [](const TerrainInputDef & acc, const TerrainInputDef & el) {
            return MakeTerrainInputDef(std::min(acc.first.first, el.first.first), std::max(acc.first.second, el.first.second),
                                       std::min(acc.second.first, el.second.first), std::max(acc.second.second, el.second.second)); 
        }
    );

    // Extend bounds by -1/+1 in the x-dimension to account for spill-over on edge cells
    bounds.first = { bounds.first.first - 1, bounds.first.second + 1 };

    // Allocate storage
    m_offset = Vec2<int>(bounds.first.first, bounds.second.first);
    m_size = (Vec2<int>(bounds.first.second, bounds.second.second) - m_offset + Vec2<int>(1, 1));
    m_count = (m_size.x * m_size.y);
    m_unoffset_max = (m_offset + m_size);

    // Populate terrain
    m_data.insert(m_data.begin(), m_count, Type::Sand);
    for (const auto & terrain : input)
    {
        for (int x = terrain.first.first; x <= terrain.first.second; ++x)
        {
            for (int y = terrain.second.first; y <= terrain.second.second; ++y)
            {
                m_data[Index(x, y)] = Type::Clay;
            }
        }
    }

    // Starting point for water generation; move down to be within the terrain area if required
    auto water = Index(Vec2<int>::Max(water_source, m_offset));
    m_data[water] = Type::FallingWater;
    m_eval.push(water);

}

void Terrain::Evaluate(void)
{
    // Get next cell for evaluation
    if (m_eval.empty()) return;
    auto cell = m_eval.top();
    m_eval.pop();
    auto state = m_data[cell];

    // Process based on cell type; static cell types can be ignored
    if (state == Type::FallingWater)
    {
        auto down = GetDown(cell);
        if (down == NO_CELL) return;    // Fell off the bottom of the terrain

        if (m_data[down] == Type::Sand)
        {
            // Fall vertically
            m_data[down] = Type::FallingWater;
            m_eval.push(down);
        }
        else if (IsSurface(down))
        {
            if (IsEnclosed(cell))
            {
                // We have reached an enclosed level; spread water out in both directions
                SpreadHorizontally(cell, Type::SettledWater, true);
            }
            else
            {
                // At a level, but not enclosed, so spread sideways where possible and propogate
                auto left = GetLeft(cell); auto right = GetRight(cell);
                if (left != NO_CELL && m_data[left] == Type::Sand) { m_data[left] = Type::FallingWater; m_eval.push(left); }
                if (left != NO_CELL && m_data[right] == Type::Sand) { m_data[right] = Type::FallingWater; m_eval.push(right); }
            }
        }

    }
}

void Terrain::EvaluateToEquilibrium(void)
{
    while (!IsAtEquilibrium())
    {
        Evaluate();
    }
}

bool Terrain::IsEnclosed(size_t pos) const
{
    // Quit immediately if we are at the terrain bottom edge (and avoid checking for this condition later), or if we have no floor
    auto down = GetDown(pos);
    if (down == NO_CELL || !IsSurface(down)) return false;

    // Iterate to the left and break when hitting a wall.  If we reach the edge, or run out of floor, this is not an enclosed cell and we can quit
    auto left = GetLeft(pos);
    while (left != NO_CELL) { 
        if (!IsSurface(GetDown(left))) return false;
        if (m_data[left] == Type::Clay) break; left = GetLeft(left); 
    }
    if (left == NO_CELL) return false;

    // Same for the right edge
    auto right = GetRight(pos);
    while (right != NO_CELL) { 
        if (!IsSurface(GetDown(right))) return false;
        if (m_data[right] == Type::Clay) break; right = GetRight(right); 
    }
    if (right == NO_CELL) return false;

    // Cell is fully-enclosed
    return true;
}

bool Terrain::IsSurface(size_t pos) const
{
    assert(pos != NO_CELL);

    auto type = m_data[pos];
    return (type == Type::Clay || type == Type::SettledWater);
}

// Spreads horizontally as far as possible.  No checks for IsEnclosed().  Adds spread cells to the eval stack if required
void Terrain::SpreadHorizontally(const size_t pos, const Terrain::Type type, const bool continue_eval)
{
    m_data[pos] = type;
    if (continue_eval) AddEval(GetUp(pos));

    auto spread = GetLeft(pos);
    while (spread != NO_CELL && m_data[spread] != Type::Clay) {
        m_data[spread] = type;
        if (continue_eval) {
            m_eval.push(spread);
            AddEval(GetUp(spread));
        }
        --spread; 
    }

    spread = GetRight(pos);
    while (spread != NO_CELL && m_data[spread] != Type::Clay) { 
        m_data[spread] = type; 
        if (continue_eval) {
            m_eval.push(spread);
            AddEval(GetUp(spread));
        }
        ++spread; 
    }
}

void Terrain::AddEval(size_t pos)
{
    if (pos != NO_CELL) m_eval.push(pos);
}


bool Terrain::IsAtEquilibrium(void) const
{
    return (m_eval.empty());
}

int Terrain::GetWaterCellCount(void) const
{
    return static_cast<int>(std::count_if(m_data.begin(), m_data.end(), [this](const Type type) { return IsWater(type); }));
}

int Terrain::GetSettledWaterCount(void) const
{
    return static_cast<int>(std::count(m_data.begin(), m_data.end(), Type::SettledWater));
}

bool Terrain::IsWater(const Type type) const
{
    return (type == Type::FallingWater || type == Type::SettledWater);
}

bool Terrain::ValidIndex(size_t index) const
{
    return (index < m_count);
}

bool Terrain::ValidCoord(const Vec2<int> & coord) const
{
    return (coord >= m_offset && coord < m_unoffset_max);
}

size_t Terrain::GetLeft(size_t index) const
{
    return (index % m_size.x != 0 ? (index - 1) : NO_CELL);
}

size_t Terrain::GetRight(size_t index) const
{
    auto right = (index + 1);
    return (right % m_size.x != 0 ? right : NO_CELL);
}

size_t Terrain::GetUp(size_t index) const
{
    return (index >= m_size.x ? (index - m_size.x) : NO_CELL);
}

size_t Terrain::GetDown(size_t index) const
{
    auto down = (index + m_size.x);
    return (down < m_count ? down : NO_CELL);
}


char Terrain::GetSchematic(Type type)
{
    switch (type)
    {
        case Type::Sand:            return '.';
        case Type::Clay:            return '#';
        case Type::SettledWater:    return '~';
        case Type::FallingWater:    return '|';
        
        default:
            assert(false);
            return 'X';
    }
}

std::string Terrain::str(void) const
{
    std::stringstream ss;

    int x = 0;
    for (auto cell : m_data)
    {
        ss << GetSchematic(cell);
        if (++x == m_size.x) { ss << '\n'; x = 0; }
    }

    return ss.str();
}

std::string Terrain::str_slice(size_t pos, int before, int after) const
{
    if (pos == NO_CELL) return "NO CELL";
    auto vec = Coord(pos);

    auto top = Vec2<int>(m_offset.x, std::max(m_offset.y, vec.y - before));
    auto bottom = Vec2<int>(m_offset.x + m_size.x - 1, std::min(m_offset.y + m_size.y - 1, vec.y + after));

    int x = 0;
    const auto start = Index(top);
    const auto end = Index(bottom);

    std::stringstream ss;
    for (size_t i = start; i <= end; ++i)
    {
        ss << GetSchematic(m_data[i]);
        if (++x == m_size.x) { ss << '\n'; x = 0; }
    }

    return ss.str();
}


