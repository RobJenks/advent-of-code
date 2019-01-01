#pragma once

#include <array>
#include <vector>
#include <unordered_set>
#include <string>
#include <memory>
#include "../common/Vec2.h"
#include "Tile.h"
#include "Actor.h"

class Combat
{
public:

    typedef std::vector<Tile> TData;
    TData Data;

    Combat(void) : m_size(0), m_count(0), m_round(0), m_terminated(true) { }
    Combat(const std::vector<std::string> & input);

    inline size_t       Index(int x, int y) const { return (x + (y * m_size.x)); }
    inline size_t       Index(const Vec2<int> & v) { return Index(v.x, v.y); }
    inline Vec2<int>    Coord(size_t index) const { return Vec2(static_cast<int>(index % m_size.x), static_cast<int>(index / m_size.x)); }

    void                Move(int actor, size_t dest);
    inline void         Move(int actor, const Vec2<int> & dest) { Move(actor, Index(dest)); }
    void                Attack(int actor, int target_actor);

    size_t              GetLeft(size_t index) const;
    size_t              GetRight(size_t index) const;
    size_t              GetUp(size_t index) const;
    size_t              GetDown(size_t index) const;
    std::array<size_t, 4U> GetAdjacent(size_t tile) const;

    std::vector<size_t> GetAdjacentEmptyTiles(size_t index) const;

    void                Execute(void);

    inline bool             HasTerminated(void) const { return m_terminated; }
    inline int              GetRoundNumber(void) const { return m_round; }
    std::unordered_multiset<int> GetRemainingActorHp(void) const;
    std::unordered_multiset<int> GetRemainingActorHpSorted(void) const;
    long                    CalculateOutcome(void) const;

    void                SetFactionAttackStrength(Actor::Class faction, int attack_strength);
    std::vector<const Actor*> GetActiveActorsOfClass(Actor::Class actor_class) const;
    std::vector<const Actor*> GetRemainingActors(void) const;

    std::string str(void) const;

private:

    int                                     AddActor(std::unique_ptr<Actor> actor);

    static const int                        PATH_INF = std::numeric_limits<int>::max();
    void                                    ResetPathfinding(void);

    void                                    PerformPathfinding(int actor);
    std::vector<std::pair<int, size_t>>     DetermineMoveOrder(void) const;
    std::vector<int>                        GetEnemyTargets(Actor::Class faction) const;
    std::vector<size_t>                     GetTilesInRangeOfTargets(const std::vector<int> & targets) const;
    size_t                                  SelectBestTarget(const std::vector<size_t> & target_locations) const;
    std::vector<size_t>                     GetShortestPathReversed(size_t target) const;
    bool                                    AttackIfPossible(int actor);


    char                                    GetSchematic(size_t index) const;
    
private:

    Vec2<int>       m_size;
    size_t          m_count;

    int             m_round;
    bool            m_terminated;

    std::vector<std::unique_ptr<Actor>>  m_actors;

};