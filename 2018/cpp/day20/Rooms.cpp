#include "Rooms.h"
#include <iostream>
#include <sstream>
#include <stack>
#include <numeric>


Rooms::Rooms(void) 
    :
    m_size(0), m_offset(0), m_unoffset_max(0), m_count(0U), m_build_backtrack(0)
{ 
}

Rooms::Rooms(const std::string & input)
    :
    m_build_backtrack(0)
{
    // Add root room
    m_rooms.push_back(Room(0, { 0, 0 }));

    // Recursive parse of input string to generate room structure
    BuildRooms(input.substr(1U, input.size() - 2U), 0);

    // Follow the raw tree to determine area bounds, assuming root room is at (0,0)
    auto[min_bounds, max_bounds] = DetermineBounds();

    // Determine room grid dimensions
    m_offset = min_bounds;
    m_size = (max_bounds - min_bounds) + Vec2<int>(1, 1);
    m_unoffset_max = (m_offset + m_size);
    m_count = (m_size.x * m_size.y);

    // Build room grid and populate from the connectivity graph
    PopulateRoomGrid();

    // Perform basic flood-fill pathfinding to determine shortest distance to all cells
    PerformPathfinding();
}


void Rooms::BuildRooms(const std::string & pattern, int parent)
{
    int current = parent;

    // Process all rooms before the next control character
    size_t start;
    for (start = 0U; start < pattern.size(); ++start)
    {
        if (pattern[start] == '(') break;
        current = CreateRoom(current, GetRoomDir(pattern[start]));
    }

    m_build_backtrack = current;

    // The pattern string may end with directional characters only
    if (start == pattern.size()) return;

    // Find the end of the string at the same scope level
    size_t end = (start + 1);
    for (int depth = 1; ; ++end)
    {
        if (pattern[end] == '(') ++depth;
        else if (pattern[end] == ')') --depth;

        if (depth == 0) break;
    }

    // We now have the substring representing this scope
    std::string scope = pattern.substr(start + 1, end - start - 1);
    while (true)
    {
        size_t option;
        int depth = 0;

        for (option = 0U; option < scope.size(); ++option)
        {
            if (scope[option] == '(') ++depth;
            else if (scope[option] == ')') --depth;
            else if (scope[option] == '|' && depth == 0) break;
        }

        BuildRooms(scope.substr(0, option), current);

        if (option == scope.size()) break;
        scope = scope.substr(option + 1);
    }

    // Process any remainder after this scope closes
    if (end < pattern.size() - 1)
    {
        BuildRooms(pattern.substr(end + 1), m_build_backtrack);
    }
}

int Rooms::CreateRoom(int prev, RoomDir traversal_dir)
{
    int id = RoomCount();

    Room room(id, m_rooms[prev], traversal_dir);
    m_rooms.push_back(room);
    return id;
}

std::pair<Vec2<int>, Vec2<int>> Rooms::DetermineBounds(void) const
{
    Vec2<int> min_bound = { +10000, +10000 };
    Vec2<int> max_bound = { -10000, -10000 };

    for (const auto & room : m_rooms)
    {
        min_bound = Vec2<int>::Min(min_bound, room.Location);
        max_bound = Vec2<int>::Max(max_bound, room.Location);
    }

    return { min_bound, max_bound };
}

void Rooms::PopulateRoomGrid(void) 
{
    m_grid.clear();
    m_grid.insert(m_grid.begin(), m_count, static_cast<int>(RoomConn::None));

    for (const auto & room : m_rooms)
    {
        auto index = Index(room.Location);
        assert(ValidCoord(room.Location) && ValidIndex(index));

        if (room.Links[static_cast<int>(RoomDir::North)] != Room::NO_ROOM) m_grid[index] |= RoomConn::North;
        if (room.Links[static_cast<int>(RoomDir::East)] != Room::NO_ROOM) m_grid[index] |= RoomConn::East;
        if (room.Links[static_cast<int>(RoomDir::South)] != Room::NO_ROOM) m_grid[index] |= RoomConn::South;
        if (room.Links[static_cast<int>(RoomDir::West)] != Room::NO_ROOM) m_grid[index] |= RoomConn::West;
    }
}

void Rooms::PerformPathfinding(void)
{
    m_distance.clear();
    m_distance.insert(m_distance.begin(), m_count, std::numeric_limits<int>::max());

    size_t root = Index(m_rooms[0].Location);

    std::stack<size_t> eval;
    eval.push(root);              // Start at the root
    m_distance[root] = 0;

    while (!eval.empty())
    {
        const auto cell = eval.top();
        eval.pop();

        const int ADJ_CONN[4] = { RoomConn::North, RoomConn::East, RoomConn::South, RoomConn::West };
        for (int i = 0; i < 4; ++i)
        {
            if (!Connects(m_grid[cell], ADJ_CONN[i])) continue;

            auto adj = GetNeighbour(cell, static_cast<RoomDir>(i));
            if (adj == Room::NO_ROOM) continue;

            auto newdist = m_distance[cell] + 1;
            if (newdist < m_distance[adj])
            {
                m_distance[adj] = newdist;
                eval.push(adj);
            }
        }
    }
    return;
}

int Rooms::GetMostDistantRoom(void) const
{
    return std::accumulate(m_distance.cbegin(), m_distance.cend(), 0, [](int acc, int el) { 
        return (el == std::numeric_limits<int>::max() ? acc : std::max(acc, el)); 
    });
}


bool Rooms::ValidIndex(size_t index) const
{
    return (index < m_count);
}

bool Rooms::ValidCoord(const Vec2<int> & coord) const
{
    return (coord >= m_offset && coord < m_unoffset_max);
}

size_t Rooms::GetLeft(size_t index) const
{
    return (index % m_size.x != 0 ? (index - 1) : NO_CELL);
}

size_t Rooms::GetRight(size_t index) const
{
    auto right = (index + 1);
    return (right % m_size.x != 0 ? right : NO_CELL);
}

size_t Rooms::GetUp(size_t index) const
{
    return (index >= m_size.x ? (index - m_size.x) : NO_CELL);
}

size_t Rooms::GetDown(size_t index) const
{
    auto down = (index + m_size.x);
    return (down < m_count ? down : NO_CELL);
}

size_t Rooms::GetNeighbour(size_t index, RoomDir dir) const
{
    switch (dir)
    {
        case RoomDir::North:        return GetUp(index);
        case RoomDir::East:         return GetRight(index);
        case RoomDir::South:        return GetDown(index);
        case RoomDir::West:         return GetLeft(index);

        default:
            assert(false);
            return Room::NO_ROOM;
    }
}

std::array<size_t, 4U> Rooms::GetAdjacent(size_t index) const
{
    return { GetUp(index), GetRight(index), GetDown(index), GetLeft(index) };
}

std::string Rooms::str_distance(void) const 
{ 
    return str([this](auto cell) { 
        auto dist = m_distance[cell]; 
        if (dist == std::numeric_limits<int>::max()) return std::string("----");
        if (dist < 10) return ".." + std::to_string(dist) + ".";
        if (dist < 100) return "." + std::to_string(dist) + ".";
        if (dist < 1000) return std::to_string(dist) + ".";
        return std::to_string(dist);
    }, 4); 
}

void Rooms::DebugPrintPartial(const std::string & for_pattern) const
{
    std::cout << "Debug partial state for \"" << for_pattern << "\":\n";
    Rooms tmp;
    
    tmp.m_rooms = m_rooms;
    auto[min_bounds, max_bounds] = tmp.DetermineBounds();

    // Determine room grid dimensions
    tmp.m_offset = min_bounds;
    tmp.m_size = (max_bounds - min_bounds) + Vec2<int>(1, 1);
    tmp.m_unoffset_max = (tmp.m_offset + tmp.m_size);
    tmp.m_count = (tmp.m_size.x * tmp.m_size.y);

    // Build room grid and populate from the connectivity graph
    tmp.PopulateRoomGrid();

    std::cout << tmp.str() << "\n";
}

