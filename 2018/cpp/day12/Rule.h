#pragma once

#include <array>
#include <sstream>
#include <cassert>
#include "../common/StringUtil.h"
#include "Value.h"

class Rule
{
public:

    std::array<Value, 5U>   Condition;
    bool                    Result;

    inline Rule(void) : Condition{ false }, Result(false) { }
    inline Rule(const std::array<Value, 5U> condition, bool result) : Condition(condition), Result(result) { }
    inline Rule(const std::string & rule)
    {
        std::string cond, result, sink;
        std::stringstream ss(rule);
        ss >> cond>> sink >> result;

        StringUtil::TrimInPlace(cond);
        assert(cond.size() == 5U);
        Condition = { cond[0] == '#', cond[1] == '#', cond[2] == '#', cond[3] == '#', cond[4] == '#' };

        StringUtil::TrimInPlace(result);
        assert(result.size() == 1U);
        Result = (result[0] == '#');
    }

    inline std::string str(void) const
    {
        std::stringstream ss;
        
        for (bool b : Condition) ss << (b ? '#' : '.');
        ss << " -> " << (Result ? '#' : '.');

        return ss.str();
    }
};