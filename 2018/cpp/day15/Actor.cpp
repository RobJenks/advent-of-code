#include "Actor.h"
#include <cassert>

Actor::Actor(Actor::Class actor_class)
    :
    m_id(0),
    m_class(actor_class),
    m_location(0U),
    m_hp(INITIAL_HP),
    m_attack(DEFAULT_ATTACK)
{
}


std::string Actor::ClassString(Actor::Class actor_class)
{
    switch (actor_class)
    {
        case Actor::Class::Elf:         return "Elf";
        case Actor::Class::Goblin:      return "Goblin";
        default:
            assert(false);
            return "Unknown";
    }
}