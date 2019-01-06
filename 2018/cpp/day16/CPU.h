#pragma once

#include <iostream>
#include "Opcode.h"
#include "Instruction.h"
#include "Registers.h"


class CPU
{
public:

    CPU(bool report_errors, bool report_execution);
    inline CPU(bool report_errors) : CPU(report_errors, false) { }
    inline CPU(void) : CPU(true) { }

    // Evaluate the given instruction against the registers provided, in-place, and optionally overriding the opcode to a specific value
    void EvaluateInPlaceAs(const Instruction & instr, int opcode, Registers & registers) const;
    
    void EvaluateInPlace(const Instruction & instr, Registers & registers) const;
    Registers EvaluateAs(const Instruction & instr, int opcode, Registers registers) const;
    Registers Evaluate(const Instruction & instr, Registers registers) const;

    Registers EvaluateProgram(Registers reg, std::vector<Instruction> instructions) const;

    inline bool HasErrors(void) const { return m_has_errors; }
    inline void ClearErrorState(void) { m_has_errors = false; }

private:

    inline void RecordError(void) const { m_has_errors = true; }

    inline bool AssertRegisterID(const Instruction & instr, const Registers & reg, int index) const
    {
        if (instr[index] >= 0 && instr[index] < reg.Count()) return true;

        RecordError();
        if (m_report_errors) std::cout << "ERROR: Value " << index << " in instruction " << instr << " is not a valid register ID\n";
        return false;
    }

    inline bool AssertRegA(const Instruction & instr, const Registers & reg) const { return AssertRegisterID(instr, reg, 1); }
    inline bool AssertRegB(const Instruction & instr, const Registers & reg) const { return AssertRegisterID(instr, reg, 2); }

#   define RequireReg(instr, reg, ix) if (!AssertRegisterID(instr, reg, ix)) return;
#   define RequireRegA(instr, reg) if (!AssertRegA(instr, reg)) return;
#   define RequireRegB(instr, reg) if (!AssertRegB(instr, reg)) return;
    
private:

    // Opcodes
    void addr(const Instruction & instr, Registers & reg) const;
    void addi(const Instruction & instr, Registers & reg) const;
    void mulr(const Instruction & instr, Registers & reg) const;
    void muli(const Instruction & instr, Registers & reg) const;
    void banr(const Instruction & instr, Registers & reg) const;
    void bani(const Instruction & instr, Registers & reg) const;
    void borr(const Instruction & instr, Registers & reg) const;
    void bori(const Instruction & instr, Registers & reg) const;
    void setr(const Instruction & instr, Registers & reg) const;
    void seti(const Instruction & instr, Registers & reg) const;
    void gtir(const Instruction & instr, Registers & reg) const;
    void gtri(const Instruction & instr, Registers & reg) const;
    void gtrr(const Instruction & instr, Registers & reg) const;
    void eqir(const Instruction & instr, Registers & reg) const;
    void eqri(const Instruction & instr, Registers & reg) const;
    void eqrr(const Instruction & instr, Registers & reg) const;

    // Directives
    int BindIP(const Instruction & instr, const Registers & reg) const;

    static const int IP_NONE = -1;

private:

    mutable bool m_has_errors;
    const bool m_report_errors;
    const bool m_report_exec;
    

};