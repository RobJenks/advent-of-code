#pragma once

#include <vector>
#include "ArmyGroup.h"


class ImmuneCombat
{
public:

    ImmuneCombat(void);
    ImmuneCombat(const ImmuneCombat & other);
    
    void AddGroup(const ArmyGroup & group);
    
    std::vector<ArmyGroup> & GetGroups(void) { return m_groups; }
    const std::vector<ArmyGroup> & GetGroups(void) const { return m_groups; }
    
    int GetFactionGroupCount(ArmyGroup::Faction faction) const;

    void Execute(void);
    void ExecuteToCompletion(void);

    inline int RoundsExecuted(void) const { return m_round; }
    bool Complete(void) const;

    int GetActiveUnitCount(void) const;
    ArmyGroup::Faction DetermineWinner(void) const;

    std::vector<std::pair<int, int>> PerformTargetSelection(void) const;
    std::vector<int> GetEffectivePowerOrder(void) const;

    void ExecuteCombat(const std::vector<std::pair<int, int>> & targets);

    void ApplyBoost(ArmyGroup::Faction faction, int boost);

    template <typename Pred>
    inline std::vector<int> GroupIndices(Pred pred) const;


    std::string str_summary(void) const;
    std::string str_detail(void) const;


private:

    std::vector<ArmyGroup> m_groups;
    int m_round;
    bool m_is_stalemate;

};


template <typename Pred>
inline std::vector<int> ImmuneCombat::GroupIndices(Pred pred) const
{
    std::vector<int> result;
    for (int i = 0; i < static_cast<int>(m_groups.size()); ++i)
        if (pred(m_groups[i])) result.push_back(i);

    return result;
}