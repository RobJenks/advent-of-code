#pragma once

#include <vector>
#include <tuple>
#include <stack>
#include "../common/Vec2.h"

class Terrain
{
public:

    typedef std::pair<std::pair<int, int>, std::pair<int, int>> TerrainInputDef;
    typedef std::vector<TerrainInputDef> TerrainInputCollection;
    inline static TerrainInputDef MakeTerrainInputDef(int xmin, int xmax, int ymin, int ymax) { return { {xmin, xmax}, {ymin, ymax} }; }

    static const size_t NO_CELL = static_cast<size_t>(0U) - static_cast<size_t>(1U);

    enum class Type { Unknown = 0, Sand, Clay, SettledWater, FallingWater };


    Terrain(const TerrainInputCollection & input, Vec2<int> water_source);

    void                Evaluate(void);
    void                EvaluateToEquilibrium(void);

    bool                IsAtEquilibrium(void) const;


    inline size_t       Index(int x, int y) const { return ((x-m_offset.x) + ((y-m_offset.y) * m_size.x)); }
    inline size_t       Index(const Vec2<int> & v) const { return Index(v.x, v.y); }
    inline Vec2<int>    Coord(size_t index) const { return m_offset + Vec2(static_cast<int>(index % m_size.x), static_cast<int>(index / m_size.x)); }

    bool                ValidIndex(size_t index) const;
    bool                ValidCoord(const Vec2<int> & coord) const;

    int                 GetWaterCellCount(void) const;
    int                 GetSettledWaterCount(void) const;

    size_t              GetLeft(size_t index) const;
    size_t              GetRight(size_t index) const;
    size_t              GetUp(size_t index) const;
    size_t              GetDown(size_t index) const;


    std::string         str(void) const;
    std::string         str_slice(size_t pos, int before, int after) const;

private:

    void                AddEval(size_t pos);
    bool                IsEnclosed(size_t pos) const;
    void                SpreadHorizontally(size_t pos, Terrain::Type type, const bool continue_eval);
    bool                IsSurface(size_t pos) const;
    bool                IsWater(Type type) const;

    static char         GetSchematic(Type type);


private:

    std::vector<Type> m_data;

    Vec2<int> m_size;           // Dimensions of the terrain area
    Vec2<int> m_offset;         // Offset to apply to all coords, e.g. test terrain has x ~= [495, 506]
    size_t m_count;             // Count of cells within the terrain
    Vec2<int> m_unoffset_max;   // Maximum bound without accounting for offset, i.e. (m_offset + m_size)
    
    std::stack<size_t> m_eval;  // Stack of cells to be evaluated based on the expanding water-front
  
};