#include "CPU.h"
#include <iostream>


CPU::CPU(bool report_errors, bool report_execution)
    :
    m_report_errors(report_errors),
    m_report_exec(report_execution), 
    m_has_errors(false)
{
}


// Evaluate the given instruction against the registers provided, in-place, and optionally overriding the opcode to a specific value
void CPU::EvaluateInPlaceAs(const Instruction & instr, int opcode, Registers & registers) const
{
    // Output value of instruction is always a register reference
    RequireReg(instr, registers, 3);

    // Switch with few conditions should be compiled directly to a jump table
    switch (opcode)
    {
        case static_cast<int>(Opcode::addr) : addr(instr, registers); break;
        case static_cast<int>(Opcode::addi) : addi(instr, registers); break;
        case static_cast<int>(Opcode::mulr) : mulr(instr, registers); break;
        case static_cast<int>(Opcode::muli) : muli(instr, registers); break;
        case static_cast<int>(Opcode::banr) : banr(instr, registers); break;
        case static_cast<int>(Opcode::bani) : bani(instr, registers); break;
        case static_cast<int>(Opcode::borr) : borr(instr, registers); break;
        case static_cast<int>(Opcode::bori) : bori(instr, registers); break;
        case static_cast<int>(Opcode::setr) : setr(instr, registers); break;
        case static_cast<int>(Opcode::seti) : seti(instr, registers); break;
        case static_cast<int>(Opcode::gtir) : gtir(instr, registers); break;
        case static_cast<int>(Opcode::gtri) : gtri(instr, registers); break;
        case static_cast<int>(Opcode::gtrr) : gtrr(instr, registers); break;
        case static_cast<int>(Opcode::eqir) : eqir(instr, registers); break;
        case static_cast<int>(Opcode::eqri) : eqri(instr, registers); break;
        case static_cast<int>(Opcode::eqrr) : eqrr(instr, registers); break;

        default:
            std::cout << "Invalid opcode received: " << opcode << "\n";
            RecordError(); break;
    }
}

void CPU::EvaluateInPlace(const Instruction & instr, Registers & registers) const
{
    EvaluateInPlaceAs(instr, instr.val[0], registers);
}

Registers CPU::EvaluateAs(const Instruction & instr, int opcode, Registers registers) const
{
    EvaluateInPlaceAs(instr, opcode, registers);
    return registers;
}

Registers CPU::Evaluate(const Instruction & instr, Registers registers) const
{
    return EvaluateAs(instr, instr.val[0], registers);
}

Registers CPU::EvaluateProgram(Registers reg, std::vector<Instruction> instructions) const
{
    int IPR = IP_NONE;       // Instruction pointer register
    int IP = 0;              // Current instruction pointer value
    
    while (IP < instructions.size())
    {
        // Write IP to register if it is bound
        if (IPR != IP_NONE)
        {
            reg[IPR] = IP;
        }

        // Process the instruction or directive
        const Instruction & instr = instructions[IP];
        if (instr.OpCode() == static_cast<int>(Opcode::DIRECTIVE_IP))
        {
            IPR = BindIP(instr, reg);
            if (m_report_exec) std::cout << "Executed directive " << instr.str_opname() << "; IPR = " << IPR << "\n";
            instructions.erase(instructions.begin() + IP);  // Directives are one-shot and should not affect the IP
            continue;
        }
        else
        {
            EvaluateInPlace(instr, reg);
        }

        // Write IPR back to IP
        if (IPR != IP_NONE)
        {
            IP = reg[IPR];
        }

        // Report state if required
        if (m_report_exec)
            std::cout << "Executed " << instr.str_opname() << "; reg = " << reg << ", IP = " << IP << "\n";

        // Next instruction
        ++IP;
    }
    
    // Return final register state
    return reg;
}


// addr (add-register): reg(C) = reg(A) + reg(B)
void CPU::addr(const Instruction & instr, Registers & reg) const
{
    RequireRegA(instr, reg);
    RequireRegB(instr, reg);

    reg[instr.OutputC()] = reg[instr.InputA()] + reg[instr.InputB()];
}

// addi (add-immediate): reg(C) = reg(A) + B
void CPU::addi(const Instruction & instr, Registers & reg) const
{
    RequireRegA(instr, reg);

    reg[instr.OutputC()] = reg[instr.InputA()] + instr.InputB();
}

// mulr (multiply-register): reg(C) = reg(A) + reg(B)
void CPU::mulr(const Instruction & instr, Registers & reg) const
{
    RequireRegA(instr, reg);
    RequireRegB(instr, reg);

    reg[instr.OutputC()] = reg[instr.InputA()] * reg[instr.InputB()];
}

// muli (multiply-immediate): reg(C) = reg(A) + B
void CPU::muli(const Instruction & instr, Registers & reg) const
{
    RequireRegA(instr, reg);

    reg[instr.OutputC()] = reg[instr.InputA()] * instr.InputB();
}

// banr (bitwise-and-register): reg(C) = reg(A) & reg(B)
void CPU::banr(const Instruction & instr, Registers & reg) const
{
    RequireRegA(instr, reg);
    RequireRegB(instr, reg);

    reg[instr.OutputC()] = reg[instr.InputA()] & reg[instr.InputB()];
}

// bani (bitwise-and-immediate): reg(C) = reg(A) & B
void CPU::bani(const Instruction & instr, Registers & reg) const
{
    RequireRegA(instr, reg);

    reg[instr.OutputC()] = reg[instr.InputA()] & instr.InputB();
}

// borr (bitwise-or-register): reg(C) = reg(A) | reg(B)
void CPU::borr(const Instruction & instr, Registers & reg) const
{
    RequireRegA(instr, reg);
    RequireRegB(instr, reg);

    reg[instr.OutputC()] = reg[instr.InputA()] | reg[instr.InputB()];
}

// bori (bitwise-or-immediate): reg(C) = reg(A) | B
void CPU::bori(const Instruction & instr, Registers & reg) const
{
    RequireRegA(instr, reg);

    reg[instr.OutputC()] = reg[instr.InputA()] | instr.InputB();
}

// setr (set-register): reg(C) = reg(A)
void CPU::setr(const Instruction & instr, Registers & reg) const
{
    RequireRegA(instr, reg);

    reg[instr.OutputC()] = reg[instr.InputA()];
}

// seti (set-immediate): reg(C) = A
void CPU::seti(const Instruction & instr, Registers & reg) const
{
    reg[instr.OutputC()] = instr.InputA();
}

// gtir (greater-than-immediate/register): reg(C) = (A > reg(B) ? 1 : 0)
void CPU::gtir(const Instruction & instr, Registers & reg) const
{
    RequireRegB(instr, reg);

    reg[instr.OutputC()] = (instr.InputA() > reg[instr.InputB()] ? 1 : 0);
}

// gtri (greater-than-register/immediate): reg(C) = (reg(A) > B ? 1 : 0)
void CPU::gtri(const Instruction & instr, Registers & reg) const
{
    RequireRegA(instr, reg);

    reg[instr.OutputC()] = (reg[instr.InputA()] > instr.InputB() ? 1 : 0);
}

// gtrr (greater-than-register/register): reg(C) = (reg(A) > reg(B) ? 1 : 0)
void CPU::gtrr(const Instruction & instr, Registers & reg) const
{
    RequireRegA(instr, reg);
    RequireRegB(instr, reg);

    reg[instr.OutputC()] = (reg[instr.InputA()] > reg[instr.InputB()] ? 1 : 0);
}

// eqir (equal-immediate/register): reg(C) = (A == reg(B) ? 1 : 0)
void CPU::eqir(const Instruction & instr, Registers & reg) const
{
    RequireRegB(instr, reg);

    reg[instr.OutputC()] = (instr.InputA() == reg[instr.InputB()] ? 1 : 0);
}

// eqri (equal-register/immediate): reg(C) = (reg(A) == B ? 1 : 0)
void CPU::eqri(const Instruction & instr, Registers & reg) const
{
    RequireRegA(instr, reg);

    reg[instr.OutputC()] = (reg[instr.InputA()] == instr.InputB() ? 1 : 0);
}

// eqrr (equal-register/register): reg(C) = (reg(A) == reg(B) ? 1 : 0)
void CPU::eqrr(const Instruction & instr, Registers & reg) const
{
    RequireRegA(instr, reg);
    RequireRegB(instr, reg);

    reg[instr.OutputC()] = (reg[instr.InputA()] == reg[instr.InputB()] ? 1 : 0);
}

// #ip (bind-IP): process a directive to bind the instruction pointer to a given register.  Returns the new IPR
int CPU::BindIP(const Instruction & instr, const Registers & reg) const
{
    if (!AssertRegA(instr, reg)) return -1;

    return instr.InputA();
}
