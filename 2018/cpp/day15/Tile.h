#pragma once
#include <vector>
#include <algorithm>
#include "../common/Vec2.h"


class Tile
{
public:

    enum class State { Empty = 0, Wall, Actor };
    enum class PathState { Unvisited = 0, Open, Visited };
    static const int NO_ACTOR = -1;
    static const size_t NO_TILE = static_cast<size_t>(0U) - static_cast<size_t>(1U);
    
    Tile(size_t id, Vec2<int> coord);
    inline Tile(void) : Tile(0U, Vec2<int>::Zero()) { }

    inline size_t GetID(void) const { return m_id; }
    inline Vec2<int> GetCoord(void) const { return m_coord; }

    void SetState(State state);
    void SetActor(int actor);
    void RemoveActor(void);

    inline State GetState(void) const { return m_state; }
    inline bool IsEmpty(void) const { return (m_state == State::Empty); }

    inline int GetActor(void) const { return m_actor; }
    inline bool HasActor(void) const { return (m_state == State::Actor); }

    void ResetPathfinding(void);    
    inline PathState GetPathState(void) const { return m_path_state; }
    inline size_t GetPathPredecessor(void) const { return m_path_prev; }
    inline int GetPathDistance(void) const { return m_path_dist; }
    inline void SetPathState(PathState state) { m_path_state = state; }
    void SetPathDetails(size_t predecessor, int dist);


    inline bool operator==(const Tile & other) const { return m_id == other.m_id; }
    inline bool operator!=(const Tile & other) const { return !(*this == other); }
    inline bool operator<(const Tile & other) const { return m_id < other.m_id; }

    inline Tile(const Tile & other) = default;
    inline Tile(Tile && other) = default;
    inline Tile & operator=(const Tile & other) = default;
    inline Tile & operator=(Tile && other) = default;

    inline ~Tile(void) { }

    static State StateFromSchematic(char schematic);

private:

    size_t          m_id;
    Vec2<int>       m_coord;

    State           m_state;
    int             m_actor;

    // Store pathfinding data in-place for convenience
    static const int PATH_INF = std::numeric_limits<int>::max();
    PathState       m_path_state;
    size_t          m_path_prev;
    int             m_path_dist;


};

inline std::ostream & operator<<(std::ostream & os, const Tile & t)
{
    os << "{ Tile " << t.GetID() << " " << t.GetCoord() << " }";
    return os;
}
