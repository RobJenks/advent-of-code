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

void ArmyGroup::TakeDamageFrom(const ArmyGroup & group)
{
    auto damage = CalculateDamageFrom(group);
    auto casualties = (damage / m_hp);

    m_units -= std::min(casualties, m_units);
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