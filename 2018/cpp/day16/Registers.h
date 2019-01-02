#pragma once

#include <array>
#include <sstream>


struct Registers
{
    typedef std::array<int, 4U> Values;
    Values val;

    inline Registers(void) : val{ 0 } { }
    inline Registers(Values && values) : val(values) { }
    inline Registers(int r0, int r1, int r2, int r3) : val{ r0,r1,r2,r3 } { }

    inline int & operator[](int ix) { return val[ix]; }
    inline int operator[](int ix) const { return val[ix]; }

    inline bool operator==(const Registers & other) const { return val == other.val; }
    inline bool operator!=(const Registers & other) const { return !(*this == other); }


    inline std::string str(void) const
    {
        std::stringstream ss;
        ss << "[ " << val[0] << " " << val[1] << " " << val[2] << " " << val[3] << " ]";
        return ss.str();
    }
};



inline std::ostream & operator<<(std::ostream & os, const Registers & reg)
{
    os << reg.str(); return os;
}
