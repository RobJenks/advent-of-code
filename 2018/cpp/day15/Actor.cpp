#include "Actor.h"

Actor::Actor(Actor::Class actor_class)
    :
    m_id(0),
    m_hp(INITIAL_HP),
    m_class(actor_class),
    m_location(0U)
{
}
