#pragma once

#include <limits>


class CPUConfig
{
public:

    static const int IP_NONE = -1;

    inline CPUConfig(void)
        :
        m_cycle_limit(std::numeric_limits<size_t>::max()),
        m_instruction_halt(std::numeric_limits<int>::max()),
        m_instruction_entry_point(0),
        m_initial_ipr(IP_NONE)
    {
    }

    inline size_t GetCycleLimit(void) const { return m_cycle_limit; }
    inline int GetInstructionHalt(void) const { return m_instruction_halt; }
    inline int GetInstructionEntryPoint(void) const { return m_instruction_entry_point; }
    inline int GetInitialIPR(void) const { return m_initial_ipr; }

    inline CPUConfig & WithCycleLimit(size_t limit) { m_cycle_limit = limit; return *this; }
    inline CPUConfig & WithInstructionHalt(int instr) { m_instruction_halt = instr; return *this; }
    inline CPUConfig & WithInstructionEntryPoint(int instr) { m_instruction_entry_point = instr; return *this; }
    inline CPUConfig & WithInitialIPR(int ipr) { m_initial_ipr = ipr; return *this; }

private:

    size_t      m_cycle_limit;
    int         m_instruction_halt;
    int         m_instruction_entry_point;
    int         m_initial_ipr;

};