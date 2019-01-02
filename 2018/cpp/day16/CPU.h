#pragma once

#include <iostream>
#include "Opcode.h"
#include "Instruction.h"
#include "Registers.h"


class CPU
{
public:

    CPU(bool report_errors);
    CPU(void) : CPU(true) { }

    // Evaluate the given instruction against the registers provided, in-place, and optionally overriding the opcode to a specific value
    void EvaluateInPlaceAs(const Instruction & instr, int opcode, Registers & registers) const;
    
    void EvaluateInPlace(const Instruction & instr, Registers & registers) const;
    Registers EvaluateAs(const Instruction & instr, int opcode, Registers registers) const;
    Registers Evaluate(const Instruction & instr, Registers registers) const;


    inline bool HasErrors(void) const { return m_has_errors; }
    inline void ClearErrorState(void) { m_has_errors = false; }

private:

    inline void RecordError(void) const { m_has_errors = true; }

    inline bool AssertRegisterID(const Instruction & instr, int index) const
    {
        if (instr[index] >= 0 && instr[index] < 4) return true;

        RecordError();
        if (m_report_errors) std::cout << "ERROR: Value " << index << " in instruction " << instr << " is not a valid register ID\n";
        return false;
    }

    inline bool AssertRegA(const Instruction & instr) const { return AssertRegisterID(instr, 1); }
    inline bool AssertRegB(const Instruction & instr) const { return AssertRegisterID(instr, 2); }

#   define RequireReg(instr, ix) if (!AssertRegisterID(instr, ix)) return;
#   define RequireRegA(instr) if (!AssertRegA(instr)) return;
#   define RequireRegB(instr) if (!AssertRegB(instr)) return;
    
private:

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


private:

    mutable bool m_has_errors;
    bool m_report_errors;
    

};