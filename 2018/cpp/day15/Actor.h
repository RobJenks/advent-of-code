#pragma once

class Actor
{
public:

    enum class Class
    {
        Elf = 0,
        Goblin = 1
    };

    Actor(Class actor_class);

    inline int GetID(void) const { return m_id; }
    inline void SetID(int id) { m_id = id; }

    inline Class GetClass(void) const { return m_class; }
    
    inline size_t GetLocation(void) const { return m_location; }
    inline void SetLocation(size_t location) { m_location = location; }

    inline int GetHP(void) const { return m_hp; }
    inline bool IsAlive(void) const { return (m_hp > 0); }
    inline void SetHP(int hp) { m_hp = hp; }

    inline int GetAttackStrength(void) const { return DEFAULT_ATTACK; }
    inline void TakeDamage(int damage) { SetHP(GetHP() - damage); }

    inline bool operator<(const Actor & other) const { return (m_location < other.m_location); }

private:

    inline static const int INITIAL_HP = 200;
    inline static const int DEFAULT_ATTACK = 3;

    int m_id;
    Class m_class;
    size_t m_location;
    int m_hp;
};



class Elf : public Actor
{
public:
    Elf(void) : Actor(Actor::Class::Elf) { }
};


class Goblin : public Actor
{
public:
    Goblin(void) : Actor(Actor::Class::Goblin) { }
};