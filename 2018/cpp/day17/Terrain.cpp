#include "Terrain.h"
#include <iostream>
#include <sstream>
#include <numeric>
#include <algorithm>
#include <cassert>


Terrain::Terrain(const TerrainInputCollection & input)
{
    // Determine terrain bounds
    TerrainInputDef bounds = std::accumulate(input.cbegin(), input.cend(), MakeTerrainInputDef(10000, -10000, 10000, -10000),
        [](const TerrainInputDef & acc, const TerrainInputDef & el) {
            return MakeTerrainInputDef(std::min(acc.first.first, el.first.first), std::max(acc.first.second, el.first.second),
                                       std::min(acc.second.first, el.second.first), std::max(acc.second.second, el.second.second)); 
        }
    );

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


