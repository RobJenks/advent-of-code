#pragma once

#include <cassert>
#include "../common/StringUtil.h"


class Damage
{
public:

    enum class Type
    {
        Unknown = 0,
        Cold,
        Radiation,
        Slashing,
        Fire,
        Bludgeoning
    };

    inline static Type FromString(const std::string & str);
    inline static std::string ToString(Type type);



};


#define SCASE(x) else if (str == #x) return Damage::Type::x
inline Damage::Type Damage::FromString(const std::string & s)
{
    std::string str = StringUtil::SentenceCase(s);
    
    if (false) {}
    SCASE(Cold);
    SCASE(Radiation);
    SCASE(Slashing);
    SCASE(Fire);
    SCASE(Bludgeoning);

    else
    {
        assert(false);
        return Damage::Type::Unknown;
    }
}
#undef SCASE

#define SCASE(x) case Damage::Type::x: return #x
inline std::string Damage::ToString(Damage::Type type)
{
    switch (type)
    {
        SCASE(Cold);
        SCASE(Radiation);
        SCASE(Slashing);
        SCASE(Fire);
        SCASE(Bludgeoning);

        default:
            assert(false);
            return "Unknown";
    }
}
#undef SCASE