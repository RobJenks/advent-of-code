#include "Combat.h"
#include <iostream>
#include <cassert>
#include <sstream>
#include <queue>
#include <numeric>
#include <algorithm>
#include "RO.h"


Combat::Combat(const std::vector<std::string> & input)
    :
    m_round(0),
    m_terminated(false)
{
    m_size = Vec2<int>(static_cast<int>(input[0].size()), static_cast<int>(input.size()));
    m_count = (m_size.x * m_size.y);

    Data.reserve(m_count);
    for (size_t i = 0U; i < m_count; ++i)
    {
        Data.push_back(std::move(Tile(i, Coord(i))));
    }

    size_t index = 0U;
    for (const auto & line : input)
    {
        for (const auto c : line)
        {
            if (c == 'E' || c == 'G')
            {
                int actor = AddActor(c == 'E' 
                    ? static_cast<std::unique_ptr<Actor>>(std::move(std::make_unique<Elf>())) 
                    : static_cast<std::unique_ptr<Actor>>(std::move(std::make_unique<Goblin>()))
                );

                Data[index].SetActor(actor);
                m_actors[actor]->SetLocation(index);
            }
            else
            {
                Data[index].SetState(Tile::StateFromSchematic(c));
            }

            ++index;
        }
    }
}

int Combat::AddActor(std::unique_ptr<Actor> actor)
{
    int actor_id = static_cast<int>(m_actors.size());

    actor->SetID(actor_id);
    m_actors.emplace_back(std::move(actor));
    
    return actor_id;
}

void Combat::Move(int actor, size_t dest)
{
    auto location = m_actors[actor]->GetLocation();
    Data[location].RemoveActor();

    m_actors[actor]->SetLocation(dest);
    Data[dest].SetActor(actor);
}

// Returns a sorted vector of pair<actor_id, actor_location> in move order for the round
std::vector<std::pair<int, size_t>> Combat::DetermineMoveOrder(void) const
{
    std::vector<std::pair<int, size_t>> sorted;
    std::transform(m_actors.cbegin(), m_actors.cend(), std::back_inserter(sorted), [](const std::unique_ptr<Actor> & el) {
        return std::make_pair(el->GetID(), el->GetLocation());
    });

    std::sort(sorted.begin(), sorted.end(), [](const auto & x0, const auto & x1) { return (x0.second < x1.second); });
    return sorted;
}

// Returns a list of enemy targets for the given faction
std::vector<int> Combat::GetEnemyTargets(Actor::Class faction) const
{
    std::vector<int> targets;
    std::for_each(m_actors.cbegin(), m_actors.cend(), [&targets, faction](const auto & el) {
        if (el->GetClass() != faction && el->IsAlive()) targets.push_back(el->GetID());
    });

    return targets;
}

// Returns a list of tiles in range of enemy targets
std::vector<size_t> Combat::GetTilesInRangeOfTargets(const std::vector<int> & targets) const
{
    std::vector<size_t> tiles;
    std::for_each(targets.cbegin(), targets.cend(), [this, &tiles](const int actor) {
        const auto adj = GetAdjacentEmptyTiles(m_actors[actor]->GetLocation());
        tiles.insert(tiles.end(), adj.begin(), adj.end());
    });

    return tiles;
}

void Combat::Execute(void)
{
    // No further execution if combat has completed
    assert(!m_terminated);

    // Evaluate actor turns in reading order
    auto actors = DetermineMoveOrder();
    for (const auto & a : actors)
    {
        Actor & actor = *m_actors[a.first].get();
        if (!actor.IsAlive()) continue;

        // If in range of a target, attack without moving and end the actor's turn
        if (AttackIfPossible(actor.GetID()))
        {
            continue;
        }

        // Determine the set of all possible targets; if there are no remaining enemies the combat ends immediately
        auto targets = GetEnemyTargets(actor.GetClass());
        if (targets.empty())
        {
            m_terminated = true;
            return;
        }

        // Find all valid & empty locations surrounding the enemy actors
        auto target_locations = GetTilesInRangeOfTargets(targets);

        // Perform pathfinding from the actor to all reachable tiles
        PerformPathfinding(actor.GetID());

        // Select the best target location based on { distance, reading-order }
        auto target = SelectBestTarget(target_locations);

        // Actor will end its turn if it has no reachable target locations
        if (target == Tile::NO_TILE) continue;
        
        // Get the shortest path to the target, with first step determined by reading order in case of a tie
        auto path = GetShortestPathReversed(target);

        // An empty result means there is no possible path to the target
        if (path.empty())
        {
            continue;
        }
        
        // Move the actor along this path one step
        Move(actor.GetID(), Coord(path.back()));

        // Attack an adjacent actor if possible, then end the turn
        AttackIfPossible(actor.GetID());
    }

    // Round is only recorded if it completed in full
    ++m_round;
}


void Combat::ResetPathfinding(void)
{
    std::for_each(Data.begin(), Data.end(), [](auto & el) { el.ResetPathfinding(); });
}


// Performs pathfinding for the given actor to all reachable tiles in the area
void Combat::PerformPathfinding(int actor)
{
    // All nodes are initially unvisited
    ResetPathfinding();

    typedef std::pair<int, int> VERT;   // { distance, id }
    std::priority_queue<VERT, std::vector<VERT>, std::greater<VERT>> open;

    // Initialise open list with starting node
    int start = static_cast<int>(m_actors[actor]->GetLocation());
    open.push({ 0, start });
    Data[start].SetPathDetails(Tile::NO_TILE, 0);

    while (!open.empty())
    {
        int node = open.top().second;
        open.pop();
        Data[node].SetPathState(Tile::PathState::Visited);

        // Consider all node neighbours
        auto adj = GetAdjacent(node);
        for (int i = 0; i < 4; ++i)
        {
            // Only consider valid, unvisited & empty cells
            if (adj[i] == Tile::NO_TILE) continue;
            Tile & nb = Data[adj[i]];
            if (!nb.IsEmpty() || nb.GetPathState() == Tile::PathState::Visited) continue;

            int newdist = Data[node].GetPathDistance() + 1;
            if (newdist < nb.GetPathDistance())
            {
                // New shortest path to this element
                nb.SetPathDetails(node, newdist);
                open.push({ newdist, static_cast<int>(adj[i]) });
            }
        }
    }
}

size_t Combat::SelectBestTarget(const std::vector<size_t> & target_locations) const
{
    if (target_locations.empty()) return Tile::NO_TILE;

    return std::accumulate(target_locations.cbegin(), target_locations.cend(), std::make_pair(Tile::NO_TILE, PATH_INF), 
        [this](const std::pair<size_t, int> acc, const size_t el) {
            return ((Data[el].GetPathDistance() < acc.second ||
                    (Data[el].GetPathDistance() == acc.second && Data[el].GetID() < acc.first)) 
                        ? std::make_pair(el, Data[el].GetPathDistance()) : acc);
    }).first;
}

// Returns the shortest path to the target location, with nodes in reverse order (target -> source)
// First step from source is tie-broken based on reading order if necessary
std::vector<size_t> Combat::GetShortestPathReversed(size_t target) const
{
    // Target is unreachable if we didn't ever visit the target
    if (Data[target].GetPathState() != Tile::PathState::Visited) return { };

    std::vector<size_t> path;
    path.reserve(Data[target].GetPathDistance());
    
    // Path should always terminate at target location
    size_t tile = target;
    path.push_back(tile);
    
    // If we are already adjacent to the target then there is no calculation required
    if (Data[tile].GetPathDistance() == 1) return path;

    // Otherwise, collect path up to distance == 2
    while (true)
    {
        tile = Data[tile].GetPathPredecessor();
        if (Data[tile].GetPathDistance() < 2) break;

        path.push_back(tile);
    }

    // Tie-break the final node (the first step from the actor) based on reading order if required
    auto adj = GetAdjacent(path.back());

    tile = Tile::NO_TILE;
    for (int i = 0; i < 4; ++i)
    {
        if (adj[i] != Tile::NO_TILE && Data[adj[i]].GetPathDistance() == 1)
        {
            tile = RO::ObjVal(tile, adj[i]);
        }
    }

    assert(tile != Tile::NO_TILE);
    path.push_back(tile);

    return path;
}

// Attacks an adjacent enemy if possible.  Returns a flag indicating whether any attack was performed
bool Combat::AttackIfPossible(int actor)
{
    Actor & act = *m_actors[actor].get();
    auto adj = GetAdjacent(act.GetLocation());

    size_t target = Tile::NO_TILE; 
    int target_hp = std::numeric_limits<int>::max();

    for (int i = 0; i < 4; ++i)
    {
        // Look for valid tiles containing actors that are our enemy.  Prioritise more injured units
        if (adj[i] != Tile::NO_TILE && Data[adj[i]].HasActor() && m_actors[Data[adj[i]].GetActor()]->GetClass() != act.GetClass())
        {
            auto hp = m_actors[Data[adj[i]].GetActor()]->GetHP();
            if (hp < target_hp)
            {
                target = adj[i];
                target_hp = hp;
            }
            else if (hp == target_hp)
            {
                target = RO::ObjVal(target, adj[i]);
                target_hp = m_actors[Data[target].GetActor()]->GetHP();
            }
        }
    }

    // Early-exit if no targets are available
    if (target == Tile::NO_TILE) return false;
    assert(Data[target].HasActor());

    // We have a target; perform the attack and return success
    Attack(actor, Data[target].GetActor());
    return true;
}

// Resolve an attack from the given actor to its target
void Combat::Attack(int actor, int target_actor)
{
    m_actors[target_actor]->TakeDamage(m_actors[actor]->GetAttackStrength());

    if (!m_actors[target_actor]->IsAlive())
    {
        Data[m_actors[target_actor]->GetLocation()].RemoveActor();
    }
}

// Returns an unordered set of the remaining actor HP values in this combat
std::unordered_multiset<int> Combat::GetRemainingActorHp(void) const
{
    std::unordered_multiset<int> hp;

    std::for_each(m_actors.cbegin(), m_actors.cend(), [&hp](const auto & el) {
        if (el->IsAlive()) hp.emplace(el->GetHP());
    });

    return hp;
}

// Returns an unordered set of the remaining actor HP values in this combat, sorted by actor position in reading order
std::unordered_multiset<int> Combat::GetRemainingActorHpSorted(void) const
{
    std::unordered_multiset<int> hp;

    std::vector<Actor*> sorted;
    std::transform(m_actors.begin(), m_actors.end(), std::back_inserter(sorted), [](const auto & el) { return el.get(); });

    std::sort(sorted.begin(), sorted.end(), [](const auto x0, const auto x1) {
        return (x0->GetLocation() < x1->GetLocation());
    });

    std::for_each(sorted.cbegin(), sorted.cend(), [&hp](const auto & el) {
        if (el->IsAlive()) hp.emplace(el->GetHP());
    });

    return hp;
}

// Returns the combat outcome: round number * sum of HP for all remaining actors
long Combat::CalculateOutcome(void) const
{
    auto remaining_actor_hp = GetRemainingActorHp();

    return
        static_cast<long>(m_round)
        *
        static_cast<long>(std::accumulate(remaining_actor_hp.cbegin(), remaining_actor_hp.cend(), 0));
}

size_t Combat::GetLeft(size_t index) const
{
    return (index % m_size.x != 0 ? (index - 1) : Tile::NO_TILE);
}

size_t Combat::GetRight(size_t index) const
{
    auto right = (index + 1);
    return (right % m_size.x != 0 ? right : Tile::NO_TILE);
}

size_t Combat::GetUp(size_t index) const
{
    return (index >= m_size.x ? (index - m_size.x) : Tile::NO_TILE);
}

size_t Combat::GetDown(size_t index) const
{
    auto down = (index + m_size.x);
    return (down < m_count ? down : Tile::NO_TILE);
}

std::array<size_t, 4U> Combat::GetAdjacent(size_t tile) const
{ 
    return { GetLeft(tile), GetUp(tile), GetRight(tile), GetDown(tile) };
}

std::vector<size_t> Combat::GetAdjacentEmptyTiles(size_t index) const
{
    auto adj = GetAdjacent(index);
    
    std::vector<size_t> tiles;
    for (size_t i = 0U; i < 4U; ++i)
    {
        if (adj[i] != Tile::NO_TILE && Data[adj[i]].IsEmpty()) tiles.push_back(adj[i]);
    }

    return tiles;
}

void Combat::SetFactionAttackStrength(Actor::Class faction, int attack_strength)
{
    std::for_each(m_actors.begin(), m_actors.end(), [faction, attack_strength](const auto & el) {
        if (el->GetClass() == faction) el->SetAttackStrength(attack_strength);
    });
}

std::vector<const Actor*> Combat::GetActiveActorsOfClass(Actor::Class actor_class) const
{
    std::vector<const Actor*> actors;
    std::for_each(m_actors.cbegin(), m_actors.cend(), [&actors, actor_class](const auto & el) {
        if (el->IsAlive() && el->GetClass() == actor_class) actors.push_back(el.get());
    });

    return actors;
}

// Returns all actors which are currently alive
std::vector<const Actor*> Combat::GetRemainingActors(void) const
{
    std::vector<const Actor*> alive;
    std::for_each(m_actors.cbegin(), m_actors.cend(), [&alive](const auto & el) {
        if (el->IsAlive()) alive.push_back(el.get());
    });

    return alive;
}



char Combat::GetSchematic(size_t index) const
{
    const auto & tile = Data[index];
    switch (tile.GetState())
    {
        case Tile::State::Empty:
            return '.';
        case Tile::State::Wall:
            return '#';
        case Tile::State::Actor:
            assert(m_actors[tile.GetActor()]->IsAlive());
            return (m_actors[tile.GetActor()]->GetClass() == Actor::Class::Elf ? 'E' : 'G');

        default:
            assert(false);
            return ' ';
    }
}

std::string Combat::str(void) const
{
    std::stringstream ss;

    size_t index = 0U;
    while (index < m_count)
    {
        ss << GetSchematic(index);

        if ((++index % m_size.x) == 0) { ss << '\n'; }
    }

    return ss.str();
}
