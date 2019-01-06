#pragma once

#include <array>
#include <vector>
#include "../common/Vec2.h"

class Forest
{
public:

    enum CellState { Open = 0, Trees, Lumberyard, None };
    static const int STATE_COUNT = static_cast<int>(CellState::None);
    static const size_t NO_CELL = static_cast<size_t>(0U) - static_cast<size_t>(1U);
    typedef std::vector<CellState> TData;

    Forest(const Vec2<int> & size);
    Forest(const Vec2<int> & size, TData data);

    void Populate(const std::vector<std::string> & input);

    void Evaluate(void);
    void Evaluate(size_t iterations);
    
    inline TData        GetData(void) const { return m_data; }

    inline size_t       Index(int x, int y) const { return (x + (y * m_size.x)); }
    inline size_t       Index(const Vec2<int> & v) const { return Index(v.x, v.y); }
    inline Vec2<int>    Coord(size_t index) const { return Vec2(static_cast<int>(index % m_size.x), static_cast<int>(index / m_size.x)); }

    size_t              GetLeft(size_t index) const;
    size_t              GetRight(size_t index) const;
    size_t              GetUp(size_t index) const;
    size_t              GetDown(size_t index) const;

    CellState           GetState(size_t index) const;

    std::array<int, STATE_COUNT>    GetStateCounts(void) const;
    int                             GetResourceValue(void) const;

    std::string str(void) const;

private:

    CellState Transform(size_t index) const;

    void GetAdjacentInner(size_t index, std::array<CellState, 8U>(&state)) const;
    void GetAdjacent(size_t index, std::array<CellState, 8U>(&state)) const;

    
    static char GetSchematic(CellState state);
    static CellState GetSchematicState(char c);

private:

    const Vec2<int> m_size;
    const size_t m_count;

    TData m_data;
    TData m_transformed;

};