#pragma once

#include <string>
#include <filesystem>
namespace fs = std::experimental::filesystem;

/*
    Base class for all AoC solutions
*/
class AOCSolution
{
public:

    // Entry point for each solution
    virtual void Run(void) const = 0;

protected:

    // Support methods
    std::string ReadInput(fs::path input) const;
    std::vector<std::string> SplitData(const std::string & s, char delim) const;
    inline std::vector<std::string> GetLines(const std::string & s) const { return SplitData(s, '\n'); }
};