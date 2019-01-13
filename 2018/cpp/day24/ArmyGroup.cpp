#include "ArmyGroup.h"

ArmyGroup::ArmyGroup(Faction faction)
    :
    m_index(-1),
    m_faction(faction),
    m_faction_group_index(0),
    m_units(0),
    m_hp(0),
    m_initiative(0),
    m_damage(0),
    m_damagetype(Damage::Type::Unknown)
{
}

ArmyGroup::ArmyGroup(Faction faction, int units, int hp, int initiative, int damage, Damage::Type damage_type,
                     const std::vector<Damage::Type> & weaknesses, const std::vector<Damage::Type> & immunities)
    :
    m_index(-1),
    m_faction(faction),
    m_faction_group_index(0),
    m_units(units),
    m_hp(hp),
    m_initiative(initiative),
    m_damage(damage),
    m_damagetype(damage_type),
    m_weaknesses(weaknesses),
    m_immunities(immunities)
{
}

ArmyGroup::ArmyGroup(const ArmyGroup & other)
    :
    m_damage(other.m_damage),
    m_damagetype(other.m_damagetype),
    m_faction(other.m_faction),
    m_faction_group_index(other.m_faction_group_index),
    m_hp(other.m_hp),
    m_immunities(other.m_immunities),
    m_index(other.m_index),
    m_initiative(other.m_initiative),
    m_units(other.m_units),
    m_weaknesses(other.m_weaknesses)
{
}

ArmyGroup & ArmyGroup::operator=(const ArmyGroup & other)
{
    m_damage = other.m_damage;
    m_damagetype = other.m_damagetype;
    m_faction = other.m_faction;
    m_faction_group_index = other.m_faction_group_index;
    m_hp = other.m_hp;
    m_immunities = other.m_immunities;
    m_index = other.m_index;
    m_initiative = other.m_initiative;
    m_units = other.m_units;
    m_weaknesses = other.m_weaknesses;

    return *this;
}


int ArmyGroup::CalculateDamageFrom(const ArmyGroup & other) const
{
    // Immune
    if (std::find(m_immunities.cbegin(), m_immunities.cend(), other.GetDamageType()) != m_immunities.cend()) 
        return 0;

    // Weak
    if (std::find(m_weaknesses.cbegin(), m_weaknesses.cend(), other.GetDamageType()) != m_weaknesses.cend())
        return (2 * other.EffectivePower());

    // No modifiers
    return other.EffectivePower();
}

// Applies damage from the specified group.  Returns the number of casualties that were taken
int ArmyGroup::TakeDamageFrom(const ArmyGroup & group)
{
    auto damage = CalculateDamageFrom(group);
    auto casualties = (damage / m_hp);

    casualties = std::min(casualties, m_units);
    m_units -= casualties;

    return casualties;
}




std::string ArmyGroup::str() const
{
    return "";
}

std::string ArmyGroup::str_summary() const
{
    std::vector<std::string> weak, immune;
    std::transform(m_weaknesses.cbegin(), m_weaknesses.cend(), std::back_inserter(weak), [](auto type) { return StringUtil::Trim(StringUtil::Lower(Damage::ToString(type))); });
    std::transform(m_immunities.cbegin(), m_immunities.cend(), std::back_inserter(immune), [](auto type) { return StringUtil::Trim(StringUtil::Lower(Damage::ToString(type))); });

    std::stringstream ss;
    ss << m_units << " units each with " << m_hp << " hit points ";

    if (!weak.empty() || !immune.empty())
    {
        ss << '(';
        if (!weak.empty()) ss << "weak to " << StringUtil::Concat(weak, ", ");
        if (!immune.empty()) ss << (weak.empty() ? "" : "; ") << "immune to " << StringUtil::Concat(immune, ", ");
        ss << ") ";
    }

    ss << "with an attack that does " << m_damage << ' ' << StringUtil::Lower(Damage::ToString(m_damagetype)) << " damage at initiative " << m_initiative;

    return ss.str();
}