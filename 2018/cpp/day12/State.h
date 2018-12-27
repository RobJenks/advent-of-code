#pragma once

#include <vector>
#include <sstream>
#include <algorithm>
#include "Rule.h"
#include "Value.h"

class State
{
public:

    State(void);
    State(const std::string & state);

    void ApplyRules(const std::vector<Rule> & rules);

    int DetermineStateScore(void) const;
    
    inline size_t GetZeroPoint(void) const { return m_zeropoint; }

    std::string str(bool zero_base) const;
    inline std::string str(void) const { return str(false); }

private:

    typedef std::vector<Value>  TData;
    TData                       m_data;
    size_t                      m_zeropoint;

    Value                       Equal(TData::const_iterator centre, const std::array<Value, 5U> & comp);
    
    void                        AddNegativeBuffer(size_t size);
    void                        AddPositiveBuffer(size_t size);

};