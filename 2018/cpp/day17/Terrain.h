#pragma once

#include <vector>
#include <tuple>
#include "../common/Vec2.h"

class Terrain
{
public:

    typedef std::pair<std::pair<int, int>, std::pair<int, int>> TerrainInputDef;
    typedef std::vector<TerrainInputDef> TerrainInputCollection;
    inline static TerrainInputDef MakeTerrainInputDef(int xmin, int xmax, int ymin, int ymax) { return { {xmin, xmax}, {ymin, ymax} }; }

    static const size_t NO_CELL = static_cast<size_t>(0U) - static_cast<size_t>(1U);

    enum class Type { Unknown = 0, Sand, Clay, SettledWater, FallingWater };


    Terrain(const TerrainInputCollection & input);


    inline size_t       Index(int x, int y) const { return ((x-m_offset.x) + ((y-m_offset.y) * m_size.x)); }
    inline size_t       Index(const Vec2<int> & v) { return Index(v.x, v.y); }
    inline Vec2<int>    Coord(size_t index) const { return m_offset + Vec2(static_cast<int>(index % m_size.x), static_cast<int>(index / m_size.x)); }

    bool                ValidIndex(size_t index) const;
    bool                ValidCoord(const Vec2<int> & coord) const;


    size_t              GetLeft(size_t index) const;
    size_t              GetRight(size_t index) const;
    size_t              GetUp(size_t index) const;
    size_t              GetDown(size_t index) const;


    std::string         str(void) const;

private:

    static char         GetSchematic(Type type);


private:

    std::vector<Type> m_data;

    Vec2<int> m_size;           // Dimensions of the terrain area
    Vec2<int> m_offset;         // Offset to apply to all coords, e.g. test terrain has x ~= [495, 506]
    size_t m_count;             // Count of cells within the terrain
    Vec2<int> m_unoffset_max;   // Maximum bound without accounting for offset, i.e. (m_offset + m_size)
    

};