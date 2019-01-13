#pragma once

#include <vector>
#include "Damage.h"


class ArmyGroup
{
public:

    enum class Faction { Unknown = 0, Immune, Infection };
    
    ArmyGroup(Faction faction);
    ArmyGroup(Faction faction, int units, int hp, int initiative, int damage, Damage::Type damage_type,
              const std::vector<Damage::Type> & weaknesses, const std::vector<Damage::Type> & immunities);
    inline ArmyGroup(void) : ArmyGroup(Faction::Unknown) { }

    ArmyGroup(const ArmyGroup & other);
    ArmyGroup & operator=(const ArmyGroup & other);

    inline int GetIndex(void) const { return m_index; }
    inline void SetIndex(int index) { m_index = index; }
    inline Faction GetFaction(void) const { return m_faction; }
    inline int GetFactionGroupIndex(void) const { return m_faction_group_index; }
    inline void SetFactionGroupIndex(int index) { m_faction_group_index = index; }

    inline bool IsActive(void) const { return (m_units != 0); }
    inline int GetUnitCount(void) const { return m_units; }
    inline int GetInitiative(void) const { return m_initiative; }
    inline int GetBaseDamage(void) const { return m_damage; }
    inline Damage::Type GetDamageType(void) const { return m_damagetype; }
    inline void IncreaseAttackPower(int boost) { m_damage += boost; }

    inline int EffectivePower(void) const { return (m_units * m_damage); }

    int CalculateDamageFrom(const ArmyGroup & other) const;
    inline int CalculateDamageTo(const ArmyGroup & other) const { return other.CalculateDamageFrom(*this); }

    int TakeDamageFrom(const ArmyGroup & group);

    std::string str() const;
    std::string str_summary() const;

private:

    // Primary stats
    int m_index;                 // In the overall group collection
    Faction m_faction;
    int m_faction_group_index;   // Group number for the faction; assigned when the unit joins combat

    int m_units;                 // Total units remaining 
    int m_hp;                    // Per unit
    int m_initiative;

    int m_damage;
    Damage::Type m_damagetype;

    std::vector<Damage::Type> m_weaknesses;
    std::vector<Damage::Type> m_immunities;

    // Derived stats
    

};