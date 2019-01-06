#pragma once

#include <unordered_map>
#include "../day16/Opcode.h"


class OpcodeMap
{
public:

    inline OpcodeMap(void) { Initialise(); }

    inline Opcode Get(const std::string & name)
    {
        const auto it = m_map.find(name);
        if (it != m_map.end()) return it->second;

        assert(false);
        return Opcode::_COUNT;
    }


private:

    inline void Initialise(void)
    {
        // Opcodes
        for (int i = 0; i < static_cast<int>(Opcode::_COUNT); ++i)
            m_map[OpcodeString(i)] = static_cast<Opcode>(i);

        // Directives
        m_map["#ip"] = Opcode::DIRECTIVE_IP;
    }



private:

    std::unordered_map<std::string, Opcode> m_map;

};