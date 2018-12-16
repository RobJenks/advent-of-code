#include "day7.h"
#include <iostream>
#include <sstream>
#include <assert.h>

void Day7::Run(void) const
{
    std::cout << "\nDay 7:\n";

    Part1();
    Part2();
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

void Day7::Part2(void) const
{
    std::vector<std::string> input = GetLines(ReadInput(fs::path("day7/input.txt")));
    std::vector<TextualDependency> textual_dependencies = ParseDependencies(input);

    std::unordered_set<char> nodes = IdentifyNodes(textual_dependencies);
    GraphData graph = BuildGraph(nodes, textual_dependencies);

    std::tuple<std::string, int> result = EvaluateGraphMultiWorker(graph, 5);
    std::cout << "Part 2 result = " << std::get<1>(result) << " secs (" << std::get<0>(result) << ")\n";
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
    std::string result = "";

    while (true)
    {
        // Gather all nodes which are ready to fire, and haven't yet
        std::vector<NodeIndex> active = GetActiveSet(graph);

        // Evaluation is complete if no further nodes can fire
        if (active.empty()) break;

        // Fire the node which appears first alphabetically
        graph.Nodes[active[0]].Fired = true;
        result += graph.Nodes[active[0]].ID;
    }

    return result;
}

std::vector<Day7::NodeIndex> Day7::GetActiveSet(const GraphData & graph) const
{
    std::vector<NodeIndex> active;

    for (size_t i = 0; i < graph.Nodes.size(); ++i)
    {
        const Node & node = graph.Nodes[i];

        if (node.Fired) continue;
        if (std::find_if(node.Dependencies.begin(), node.Dependencies.end(),
            [&graph](const NodeIndex dep) { return (!graph.Nodes[dep].Fired); }) != node.Dependencies.end()) continue;

        active.push_back(i);
    }

    std::sort(active.begin(), active.end(), [&graph](NodeIndex x0, NodeIndex x1) { 
        return (graph.Nodes[x0].ID < graph.Nodes[x1].ID); 
    });

    return active;
}

// Returns a tuple of the evaluated graph order and the total evaluation time.  Accepts the 
// number of available workers as an input.  Assumes all workers are homogeneous
std::tuple<std::string, int> Day7::EvaluateGraphMultiWorker(GraphData & graph, int worker_count) const
{
    std::string evaluated = "";

    // Vector holding the current job index of each worker
    std::vector<NodeIndex> workers;
    workers.insert(workers.begin(), worker_count, NO_NODE);

    // Process the graph in time slices of one second
    int time = 0;
    for (time = 0; ; ++time)
    {
        // Process all outstanding work
        int free_workers = 0;
        for (int i = 0; i < workers.size(); ++i)
        {
            NodeIndex job = workers[i];
            if (job != NO_NODE)
            {
                if (--graph.Nodes[job].TimeRemaining != 0)
                {
                    // Job still in progress
                    continue;                           
                }

                // Job done
                graph.Nodes[workers[i]].Fired = true;   // Fire on completion
                workers[i] = NO_NODE;                   
            }

            ++free_workers;
        }

        // Save cycles where all workers are busy
        if (free_workers == 0) continue;

        // Get the potential active set (excluding any jobs in progress) and assign all remaining jobs
        std::vector<NodeIndex> active = GetActiveSet(graph);
        active.erase(std::remove_if(active.begin(), active.end(), 
            [&graph](NodeIndex index) { return (graph.Nodes[index].InProgress); }
        ), active.end());

        // If there are no available jobs, and all workers are idle, we have completed evaluation
        if (active.empty() && free_workers == workers.size()) break;

        // Assign jobs to idle workers
        int current_node = 0;
        for (int i = 0; i < workers.size(); ++i)
        {
            if (workers[i] == NO_NODE)
            {
                // Assign work
                if (current_node >= active.size()) break;     // We may have jobs < workers

                workers[i] = active[current_node];
                graph.Nodes[workers[i]].InProgress = true;

                evaluated += graph.Nodes[active[current_node]].ID;
                ++current_node;
            }
        }
    }

    return { evaluated, time };
}