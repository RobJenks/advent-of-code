#pragma once

#include <string>
#include <cassert>


enum class Opcode : int
{
    // Opcodes
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

    _COUNT,

    // Directives
    DIRECTIVE_IP    // Directive to update the instruction pointer register, introduced in day 19
};


#define CaseOp(x) case Opcode::x:       return #x;
inline std::string OpcodeString(Opcode op)
{
    switch (op)
    {
        // Opcodes
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

        // Directives
        case Opcode::DIRECTIVE_IP:  return "#ip";

    default:
        assert(false && "Invalid opcode");
        return "<UNKNOWN>";
    }
}
#undef CaseOp

inline std::string OpcodeString(int op) { return OpcodeString(static_cast<Opcode>(op)); }

