#include "Tile.h"
#include <cassert>

Tile::Tile(size_t id, Vec2<int> coord)
    :
    m_id(id),
    m_coord(coord), 
    m_state(State::Empty),
    m_actor(NO_ACTOR), 
    m_path_state(PathState::Unvisited),
    m_path_prev(NO_TILE),
    m_path_dist(0)
{
}


void Tile::SetState(State state)
{
    assert(state != State::Actor);
    m_state = state;
}

void Tile::SetActor(int actor)
{
    m_state = State::Actor;
    m_actor = actor;
}

void Tile::RemoveActor(void)
{
    m_state = State::Empty;
    m_actor = NO_ACTOR;
}

void Tile::ResetPathfinding(void)
{
    m_path_state = PathState::Unvisited;
    m_path_prev = NO_TILE;
    m_path_dist = PATH_INF;
}

void Tile::SetPathDetails(size_t predecessor, int dist) 
{ 
    m_path_prev = predecessor; 
    m_path_dist = dist; 
}



Tile::State Tile::StateFromSchematic(char schematic)
{
    switch (schematic)
    {
        case '.':       
            return State::Empty;
        case '#':       
            return State::Wall;
        case 'E':
        case 'G':
            return State::Actor;
        default:
            assert(false);
            return State::Empty;
    }
}
