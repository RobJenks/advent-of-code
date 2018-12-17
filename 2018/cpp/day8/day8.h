#pragma once

#include "../base/AOCSolution.h"
#include <memory>
#include "Tree.h"


class Day8 : public AOCSolution
{
public:

    // Entry point
    void Run(void) const;


private:

    void Part1(void) const;
    
    std::unique_ptr<Tree> BuildTree(const std::string & input) const;
    int AggregateMetadata(const Tree *tree) const;

};