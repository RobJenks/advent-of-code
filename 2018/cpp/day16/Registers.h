#pragma once

#include <vector>
#include <sstream>
#include <cassert>

struct Registers
{

private:

    const int COUNT;

public:

    typedef std::vector<int> Values;
    Values val;

    inline Registers(int register_count) : COUNT(register_count) { val.insert(val.begin(), register_count, 0); }
    inline Registers(int register_count, Values && values) : COUNT(register_count), val(values) { assert(val.size() == COUNT); }

    inline int & operator[](int ix) { return val[ix]; }
    inline int operator[](int ix) const { return val[ix]; }

    inline bool operator==(const Registers & other) const { return val == other.val; }
    inline bool operator!=(const Registers & other) const { return !(*this == other); }


    inline std::string str(void) const
    {
        std::stringstream ss;
        
        ss << "[ ";
        for (int i = 0; i < COUNT; ++i) ss << val[i] << ' ';
        ss << " ]";

        return ss.str();
    }
    
};


inline std::ostream & operator<<(std::ostream & os, const Registers & reg)
{
    os << reg.str(); return os;
}
