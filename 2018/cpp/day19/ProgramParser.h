#pragma once

#include "../common/StringUtil.h"
#include "../day16/Instruction.h"
#include "OpcodeMap.h"

class ProgramParser
{
public:

    inline static std::vector<Instruction> Parse(const std::vector<std::string> & input);


};

inline std::vector<Instruction> ProgramParser::Parse(const std::vector<std::string> & input)
{
    OpcodeMap opcodes;
    std::vector<Instruction> instructions;

    for (const auto & in : input)
    {
        auto line = StringUtil::Trim(in);
        if (line.empty()) continue;

        Instruction instr;
        std::string opcode;;
        std::stringstream ss(line);

        ss >> opcode;
        instr.val[0] = static_cast<int>(opcodes.Get(opcode));

        ss >> instr.val[1] >> instr.val[2] >> instr.val[3];
        instructions.push_back(instr);
    }

    return instructions;
}