#pragma once

#include <limits>


class CPUConfig
{
public:

    inline CPUConfig(void)
        :
        m_cycle_limit(std::numeric_limits<size_t>::max()),
        m_instruction_halt(std::numeric_limits<int>::max())
    {
    }

    inline size_t GetCycleLimit(void) const { return m_cycle_limit; }
    inline int GetInstructionHalt(void) const { return m_instruction_halt; }

    inline CPUConfig & WithCycleLimit(size_t limit) { m_cycle_limit = limit; return *this; }
    inline CPUConfig & WithInstructionHalt(int instr) { m_instruction_halt = instr; return *this; }

private:

    size_t      m_cycle_limit;
    int         m_instruction_halt;

};