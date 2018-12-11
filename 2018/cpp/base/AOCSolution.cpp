#include "AOCSolution.h"
#include <sstream>
#include <fstream>
#include <filesystem>
namespace fs = std::experimental::filesystem;

std::string AOCSolution::ReadInput(fs::path input) const
{
    std::ifstream ifs(input.c_str());
    std::stringstream ss;
    
    ss << ifs.rdbuf();
    ifs.close();

    return ss.str();
}

std::vector<std::string> AOCSolution::SplitData(const std::string & s, char delim) const
{
    std::string line;
    std::vector<std::string> data;

    std::istringstream iss(s);
    while (std::getline(iss, line, delim)) {
        data.push_back(line);
    }

    return data;
}