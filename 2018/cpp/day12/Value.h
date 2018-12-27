#pragma once

// Wrapper around bool to prevent the vector<bool> bitstring specialisation (which prevents efficient
// dereferencing and STL iterator operations)
class Value
{
public:

    inline Value(void) : m_value(false) { }
    inline Value(bool value) : m_value(value) { }
    
    inline operator bool() const { return m_value; }

    inline bool operator==(const Value & other) const { return m_value == other.m_value; }

    inline Value(const Value & other) : m_value(other.m_value) { }
    inline Value(Value && other) : m_value(other.m_value) { }

    inline Value & operator=(const Value & other) { m_value = other.m_value; return *this; }
    inline Value & operator=(Value && other) { m_value = other.m_value; return *this; }

    inline ~Value(void) { }

private:

    bool m_value;

};