#include "ImmuneCombat.h"
#include <algorithm>
#include <numeric>
#include <cassert>


ImmuneCombat::ImmuneCombat(void)
    :
    m_round(0),
    m_is_stalemate(false)
{
}

ImmuneCombat::ImmuneCombat(const ImmuneCombat & other) 
    :
    m_groups(other.m_groups),
    m_round(other.m_round),
    m_is_stalemate(other.m_is_stalemate)
{
}


void ImmuneCombat::AddGroup(const ArmyGroup & group)
{
    assert(group.GetFaction() != ArmyGroup::Faction::Unknown);

    m_groups.push_back(group);
    m_groups.back().SetIndex(static_cast<int>(m_groups.size()) - 1);
    m_groups.back().SetFactionGroupIndex(GetFactionGroupCount(group.GetFaction()));
}


int ImmuneCombat::GetFactionGroupCount(ArmyGroup::Faction faction) const
{
    return static_cast<int>(std::count_if(m_groups.cbegin(), m_groups.cend(), [faction](const ArmyGroup & group) {
        return (group.IsActive() && group.GetFaction() == faction);
    }));
}

void ImmuneCombat::Execute(void)
{
    ++m_round;

    auto targets = PerformTargetSelection();
    ExecuteCombat(targets);
}

void ImmuneCombat::ExecuteToCompletion(void)
{
    while (!Complete())
    {
        Execute();
    }
}

std::vector<std::pair<int, int>> ImmuneCombat::PerformTargetSelection(void) const
{
    // Groups will perform target selection in { EffectivePower, Initiative } order
    auto order = GetEffectivePowerOrder();

    // Return targeting data per group: { target, targeted_by }
    std::vector<std::pair<int, int>> tgt;    
    tgt.insert(tgt.begin(), m_groups.size(), { -1, -1 });

    // Construct targeting data for each group in order
    for (auto index : order)
    {
        const auto & group = m_groups[index];
        auto target = std::accumulate(m_groups.cbegin(), m_groups.cend(), std::make_pair<int, int>(-1, 0), 
            [&group, &tgt, this](std::pair<int, int> acc, const ArmyGroup & el) 
        {
            if (el.GetFaction() == group.GetFaction()) return acc;  // Same team
            if (!el.IsActive()) return acc;                         // Group is dead
            if (tgt[el.GetIndex()].second != -1) return acc;        // Group has already been targeted

            int damage = group.CalculateDamageTo(el);
            if (damage == 0) return acc;                            // Do not attempt to target if dmg=0 (it would prevent other groups choosing the target)

            if (damage > acc.second) return std::make_pair<int, int>(el.GetIndex(), damage+0);
            else if (damage == acc.second)
            {
                if (el.EffectivePower() > m_groups[acc.first].EffectivePower()) return std::make_pair<int, int>(el.GetIndex(), damage+0);
                else if (el.EffectivePower() == m_groups[acc.first].EffectivePower())
                {
                    return (el.GetInitiative() > m_groups[acc.first].GetInitiative() ? std::make_pair<int, int>(el.GetIndex(), damage+0) : acc);
                }
            }

            return acc;
        });

        tgt[index].first = target.first;                            // Group -> target
        if (target.first != -1) tgt[target.first].second = index;   // Target <- being targeted
    }

    return tgt;
}

void ImmuneCombat::ExecuteCombat(const std::vector<std::pair<int, int>> & targets)
{
    // Combat proceeds in initiative order
    std::vector<std::pair<int, int>> order;
    std::transform(m_groups.cbegin(), m_groups.cend(), std::back_inserter(order), [](const auto & el) { return std::make_pair<int,int>( el.GetIndex(), el.GetInitiative() ); });
    std::sort(order.begin(), order.end(), [](const auto & x0, const auto & x1) { return (x0.second > x1.second); });

    // Evaluate combat
    int total_casualties = 0;
    for (const auto & ord : order)
    {
        auto index = ord.first;
        const auto & group = m_groups[index];

        // Group may have been destroyed by an attack earlier in the combat sequence
        if (!group.IsActive()) continue;

        // Group may not have selected any target (e.g. if its only possible targets are immune to its damage)
        auto & tgt = targets[index].first;
        if (tgt == -1) continue;

        auto & target_group = m_groups[tgt];
        total_casualties += target_group.TakeDamageFrom(group);
    }

    if (total_casualties == 0) m_is_stalemate = true;
}

std::vector<int> ImmuneCombat::GetEffectivePowerOrder(void) const
{
    std::vector<std::tuple<int, int, int>> data;    // Index, EffectivePower, Initiative
    for (int i = 0; i < static_cast<int>(m_groups.size()); ++i)
    {
        data.push_back({ i, m_groups[i].EffectivePower(), m_groups[i].GetInitiative() });
    }

    std::sort(data.begin(), data.end(), [](const auto & x0, const auto & x1) {
        return (std::get<1>(x0) > std::get<1>(x1) ||
               (std::get<1>(x0) == std::get<1>(x1) && std::get<2>(x0) > std::get<2>(x1)));
    });

    std::vector<int> result;
    std::transform(data.cbegin(), data.cend(), std::back_inserter(result), [](const auto & el) { return std::get<0>(el); });

    return result;
}

bool ImmuneCombat::Complete(void) const
{
    return (m_is_stalemate ||
            GetFactionGroupCount(ArmyGroup::Faction::Immune) == 0 ||
            GetFactionGroupCount(ArmyGroup::Faction::Infection) == 0);
}

void ImmuneCombat::ApplyBoost(ArmyGroup::Faction faction, int boost)
{
    std::for_each(m_groups.begin(), m_groups.end(), [boost, faction](auto & group) {
        if (group.GetFaction() == faction) group.IncreaseAttackPower(boost);
    });
}

int ImmuneCombat::GetActiveUnitCount(void) const
{
    return std::accumulate(m_groups.cbegin(), m_groups.cend(), 0, [](int acc, const ArmyGroup & el) {
        return (acc + el.GetUnitCount());
    });
}

// Returns the faction that has won the combat, or "Unknown" if units from both sides are still present
ArmyGroup::Faction ImmuneCombat::DetermineWinner(void) const
{
    auto active = std::accumulate(m_groups.cbegin(), m_groups.cend(), std::make_pair<int,int>(0,0), 
        [](const std::pair<int, int> & acc, const ArmyGroup & el)
    {
        if (!el.IsActive()) return acc;
        return (el.GetFaction() == ArmyGroup::Faction::Immune ? std::make_pair<int, int>(acc.first + 1, acc.second + 0)
                                                              : std::make_pair<int, int>(acc.first + 0, acc.second + 1));
    });

    if (active.first != 0)
    {
        return (active.second == 0 ? ArmyGroup::Faction::Immune : ArmyGroup::Faction::Unknown);
    }
    else
    {
        return (active.second != 0 ? ArmyGroup::Faction::Infection : ArmyGroup::Faction::Unknown);
    }
}



std::string ImmuneCombat::str_summary(void) const
{
    std::stringstream ss;

    ss << "Immune System:\n";
    if (GetFactionGroupCount(ArmyGroup::Faction::Immune) == 0) ss << "No groups remain.\n";
    else std::for_each(m_groups.cbegin(), m_groups.cend(), [&ss](const ArmyGroup & group) {
        if (group.IsActive() && group.GetFaction() == ArmyGroup::Faction::Immune) 
            ss << "Group " << group.GetFactionGroupIndex() << " contains " << group.GetUnitCount() << " units\n";
    });

    ss << "Infection:\n";
    if (GetFactionGroupCount(ArmyGroup::Faction::Infection) == 0) ss << "No groups remain.\n";
    else std::for_each(m_groups.cbegin(), m_groups.cend(), [&ss](const ArmyGroup & group) {
        if (group.IsActive() && group.GetFaction() == ArmyGroup::Faction::Infection) 
            ss << "Group " << group.GetFactionGroupIndex() << " contains " << group.GetUnitCount() << " units\n";
    });

    return ss.str();
}

std::string ImmuneCombat::str_detail(void) const
{
    std::stringstream ss;

    ss << "Immune System:\n";
    if (GetFactionGroupCount(ArmyGroup::Faction::Immune) == 0) ss << "No groups remain.\n";
    std::for_each(m_groups.cbegin(), m_groups.cend(), [&ss](const ArmyGroup & group) {
        if (group.IsActive() && group.GetFaction() == ArmyGroup::Faction::Immune) ss << group.str_summary() << "\n";
    });


    ss << "\nInfection:\n";
    if (GetFactionGroupCount(ArmyGroup::Faction::Infection) == 0) ss << "No groups remain.\n";
    std::for_each(m_groups.cbegin(), m_groups.cend(), [&ss](const ArmyGroup & group) {
        if (group.IsActive() && group.GetFaction() == ArmyGroup::Faction::Infection) ss << group.str_summary() << "\n";
    });

    return ss.str();
}