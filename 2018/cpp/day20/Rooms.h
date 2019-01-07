#pragma once

#include <string>
#include <vector>
#include "../common/Vec2.h"
#include "Room.h"
#include "RoomDir.h"


class Rooms
{
public:

    typedef int RoomCell;
    
    Rooms(void);
    Rooms(const std::string & input);


    inline size_t       Index(int x, int y) const { return ((x - m_offset.x) + ((y - m_offset.y) * m_size.x)); }
    inline size_t       Index(const Vec2<int> & v) const { return Index(v.x, v.y); }
    inline Vec2<int>    Coord(size_t index) const { return m_offset + Vec2(static_cast<int>(index % m_size.x), static_cast<int>(index / m_size.x)); }

    bool                ValidIndex(size_t index) const;
    bool                ValidCoord(const Vec2<int> & coord) const;

    static const size_t NO_CELL = static_cast<size_t>(0U) - static_cast<size_t>(1U);
    size_t              GetLeft(size_t index) const;
    size_t              GetRight(size_t index) const;
    size_t              GetUp(size_t index) const;
    size_t              GetDown(size_t index) const;
    size_t              GetNeighbour(size_t index, RoomDir dir) const;
    std::array<size_t, 4U> GetAdjacent(size_t index) const;

    int RoomCount(void) const { return static_cast<int>(m_rooms.size()); }
    int GetMostDistantRoom(void) const;

    const std::vector<int> & GetDistanceMap(void) const { return m_distance; }

    template <typename TCellLambda>
    std::string str(TCellLambda cl, int cell_size = 1) const;

    inline std::string str(void) const { return str([this](auto cell) { return (!m_rooms.empty() && cell == Index(m_rooms[0].Location) ? 'X' : '.'); }, 1); }
    std::string str_distance(void) const;
    

private:

    void BuildRooms(const std::string & pattern, int parent);
    int CreateRoom(int prev, RoomDir traversal_dir);

    std::pair<Vec2<int>, Vec2<int>> DetermineBounds(void) const;
    void PopulateRoomGrid(void);

    void PerformPathfinding(void);

    inline bool Connects(int connectivity, int direction) const { return ((connectivity & direction) == direction); }

    void DebugPrintPartial(const std::string & for_pattern) const;

private:

    std::vector<Room> m_rooms;      // Graph of room connections
    std::vector<RoomCell> m_grid;   // Resolved connectivity grid
    std::vector<int> m_distance;    // Computed shortest path to each grid cell

    Vec2<int> m_size;           // Dimensions of the room grid
    Vec2<int> m_offset;         // Offset from origin, so that the left/top-most room is actually at grid[0]
    Vec2<int> m_unoffset_max;   // Max bound of the grid without offset applied
    size_t m_count;             // Total grid cell count

    int m_build_backtrack;      // Persistent reference to the current backtrack point during recursive room build
};


template <typename TCellLambda>
std::string Rooms::str(TCellLambda cl, int cell_size) const
{
    std::stringstream ss;

    size_t index = 0U;
    Vec2<int> root = (m_offset * -1);

    for (int y = 0; y < m_size.y; ++y)
    {
        // Row 1
        for (int x = 0; x < m_size.x; ++x)
        {
            const auto cell = m_grid[index];
            ss << '#' << (Connects(cell, RoomConn::North) ? '-' : '#');
            for (int i = 1; i < cell_size; ++i) ss << '#';
            ++index;
        }
        ss << "#\n";

        // Row 2
        index -= m_size.x;
        for (int x = 0; x < m_size.x; ++x)
        {
            const auto cell = m_grid[index];
            ss << (Connects(cell, RoomConn::West) ? '|' : '#') << cl(index);
            ++index;
        }
        ss << "#\n";
    }

    // Bottom row
    std::string border(1 + cell_size, '#');
    for (int x = 0; x < m_size.x; ++x) ss << border;
    ss << "#\n";

    return ss.str();
}