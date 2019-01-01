#pragma once

#include <string>
#include <cctype>
#include <sstream>
#include <algorithm>


class StringUtil
{
public:

    inline static void TrimLeftInPlace(std::string & s) 
    {
        s.erase(s.begin(), std::find_if(s.begin(), s.end(), [](int ch) {
            return !std::isspace(ch);
        }));
    }

    inline static void TrimRightInPlace(std::string & s) 
    {
        s.erase(std::find_if(s.rbegin(), s.rend(), [](int ch) {
            return !std::isspace(ch);
        }).base(), s.end());
    }

    inline static void TrimInPlace(std::string & s) 
    {
        TrimLeftInPlace(s);
        TrimRightInPlace(s);
    }

    inline static std::string TrimLeft(std::string s) 
    {
        TrimLeftInPlace(s);
        return s;
    }

    inline static std::string TrimRight(std::string s) 
    {
        TrimRightInPlace(s);
        return s;
    }

    inline static std::string Trim(std::string s) 
    {
        TrimLeftInPlace(s);
        TrimRightInPlace(s);
        return s;
    }

    inline static std::string Concat(const std::vector<std::string> & strings, const std::string & delimiter = "\n")
    {
        std::stringstream ss;

        for (const auto & s : strings)
        {
            ss << s << '\n';
        }

        return ss.str();
    }
};