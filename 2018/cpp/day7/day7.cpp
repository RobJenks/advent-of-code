#include "day7.h"
#include <iostream>
#include <sstream>
#include <assert.h>

void Day7::Run(void) const
{
    std::cout << "\nDay 7:\n";

    Part1();
}

void Day7::Part1(void) const
{
    std::vector<std::string> input = GetLines(ReadInput(fs::path("day7/input.txt")));
    std::vector<TextualDependency> textual_dependencies = ParseDependencies(input);

    std::unordered_set<char> nodes = IdentifyNodes(textual_dependencies);
    GraphData graph = BuildGraph(nodes, textual_dependencies);

    std::string evaluated = EvaluateGraph(graph);
    std::cout << "Part 1 result = " << evaluated << "\n";
}


std::vector<Day7::TextualDependency> Day7::ParseDependencies(const std::vector<std::string> & input) const
{
    std::vector<TextualDependency> dependencies;
    std::transform(input.begin(), input.end(), std::back_inserter(dependencies), 
        [this](const auto & el) { return this->ParseDependency(el); }
    );

    return dependencies;
}

Day7::TextualDependency Day7::ParseDependency(const std::string & input) const
{
    std::stringstream ss(input);
    std::string sink;

    TextualDependency dependency;
    ss >> sink >> dependency.Requires >> sink >> sink >> sink >> sink >> sink >> dependency.Node;

    return dependency;
}

std::unordered_set<char> Day7::IdentifyNodes(const std::vector<TextualDependency> & dependencies) const
{
    std::unordered_set<char> nodes;
    std::for_each(dependencies.begin(), dependencies.end(), 
        [&nodes](const auto & el) { nodes.emplace(el.Node); nodes.emplace(el.Requires); }
    );

    return nodes;
}

Day7::GraphData Day7::BuildGraph(const std::unordered_set<char> & nodes, const std::vector<TextualDependency> & dependencies) const
{
    GraphData graph;

    // Build node collection
    for (const auto & node : nodes)
    {
        graph.Nodes.push_back(Node(node));
        graph.NodeMapping[node] = (graph.Nodes.size() - 1);
    }

    // Add dependencies
    for (const auto & depend : dependencies)
    {
        graph.GetNode(depend.Node).Dependencies.push_back(graph.GetNodeIndex(depend.Requires));
    }

    return graph;
}

std::string Day7::EvaluateGraph(GraphData & graph) const
{
    std::vector<NodeIndex> active;
    std::string result = "";

    while (true)
    {
        // Gather all nodes which are ready to fire, and haven't yet
        active.clear();
        for (size_t i = 0; i < graph.Nodes.size(); ++i)
        {
            const Node & node = graph.Nodes[i];

            if (node.Fired) continue;
            if (std::find_if(node.Dependencies.begin(), node.Dependencies.end(),
                [&graph](const NodeIndex dep) { return (!graph.Nodes[dep].Fired); }) != node.Dependencies.end()) continue;

            active.push_back(i);
        }

        // Evaluation is complete if no further nodes can fire
        if (active.empty()) break;

        // Fire the node which appears first alphabetically
        NodeIndex fire = active[0];
        for (size_t i = 1; i < active.size(); ++i)
        {
            if (graph.Nodes[active[i]].ID < graph.Nodes[fire].ID)
                fire = active[i];
        }

        graph.Nodes[fire].Fired = true;
        result += graph.Nodes[fire].ID;
    }

    return result;
}
