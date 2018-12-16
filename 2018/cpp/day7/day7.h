#pragma once

#include "../base/AOCSolution.h"
#include <string>
#include <vector>
#include <unordered_set>
#include <unordered_map>

class Day7 : public AOCSolution
{
public:

    // Entry point
    void Run(void) const;


private:

    struct TextualDependency
    {
        char Node, Requires;
        TextualDependency(void) : Node(0), Requires(0) { }
        TextualDependency(char node, char requires) : Node(node), Requires(requires) { }
    };

    typedef size_t NodeIndex;
    struct Node
    {
        char ID;
        bool Fired;
        std::vector<NodeIndex> Dependencies;

        Node(void) : Node(0) { }
        Node(char id) : ID(id), Fired(false) { }
    };

    struct GraphData
    {
        std::vector<Node> Nodes;
        std::unordered_map<char, NodeIndex> NodeMapping;

        inline NodeIndex GetNodeIndex(char id) { return NodeMapping[id]; }
        inline Node & GetNode(char id) { return Nodes[GetNodeIndex(id)]; }
    };

private:

    void Part1(void) const;


    std::vector<TextualDependency> ParseDependencies(const std::vector<std::string> & input) const;
    TextualDependency ParseDependency(const std::string & input) const;
    std::unordered_set<char> IdentifyNodes(const std::vector<TextualDependency> & dependencies) const;

    GraphData BuildGraph(const std::unordered_set<char> & nodes, const std::vector<TextualDependency> & dependencies) const;
    std::string EvaluateGraph(GraphData & graph) const;
};