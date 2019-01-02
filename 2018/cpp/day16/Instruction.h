#pragma once

#include <array>
#include <sstream>
#include "Opcode.h"

struct Instruction
{
    typedef std::array<int, 4U> Values;
    Values val;

    inline Instruction(void) : val{ 0 } { }
    inline Instruction(Values && values) : val(values) { }
    inline Instruction(int op, int a, int b, int c) : val{ op,a,b,c } { }
    inline Instruction(Opcode op, int a, int b, int c) : val{ static_cast<int>(op),a,b,c } { }


    inline int OpCode(void) const { return val[0]; }
    inline int InputA(void) const { return val[1]; }
    inline int InputB(void) const { return val[2]; }
    inline int OutputC(void) const { return val[3]; }

    inline int & operator[](int ix) { return val[ix]; }
    inline int operator[](int ix) const { return val[ix]; }

    inline std::string str(void) const
    {
        std::stringstream ss;
        ss << "{ " << val[0] << " " << val[1] << " " << val[2] << " " << val[3] << " }";
        return ss.str();
    }

    inline std::string str_opname(void) const
    {
        std::stringstream ss;
        ss << "{ " << OpcodeString(val[0]) << " " << val[1] << " " << val[2] << " " << val[3] << " }";
        return ss.str();
    }
};

inline std::ostream & operator<<(std::ostream & os, const Instruction & instr)
{
    os << instr.str(); return os;
}