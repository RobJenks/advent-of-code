#pragma once

#include <string>
#include <cassert>


enum class Opcode : int
{
    addr,
    addi,

    mulr,
    muli,

    banr,
    bani,

    borr,
    bori,

    setr,
    seti,

    gtir,
    gtri,
    gtrr,

    eqir,
    eqri,
    eqrr,

    _COUNT
};


#define CaseOp(x) case Opcode::x:       return #x;
inline std::string OpcodeString(Opcode op)
{
    switch (op)
    {
        CaseOp(addr)
        CaseOp(addi)
        CaseOp(mulr)
        CaseOp(muli)
        CaseOp(banr)
        CaseOp(bani)
        CaseOp(borr)
        CaseOp(bori)
        CaseOp(setr)
        CaseOp(seti)
        CaseOp(gtir)
        CaseOp(gtri)
        CaseOp(gtrr)
        CaseOp(eqir)
        CaseOp(eqri)
        CaseOp(eqrr)

    default:
        assert(false && "Invalid opcode");
        return "<UNKNOWN>";
    }
}
#undef CaseOp

inline std::string OpcodeString(int op) { return OpcodeString(static_cast<Opcode>(op)); }

