#include <iostream>
#include <vector>
#include <queue>

#include "benchmark.h"
#include "bitmap.h"
#include "builder.h"
#include "command_line.h"
#include "graph.h"
#include "platform_atomics.h"
#include "pvector.h"
#include "sliding_queue.h"
#include "timer.h"
#include "types.h"

using namespace std;

pvector<NodeID> DOBFS(const Graph &g, NodeID source) {

  queue<NodeID> frontier;
  frontier.push(source);

  pvector<NodeID> parent(g.num_nodes(), INVALID_NODE_ID);
  parent[source] = source;

  while (!frontier.empty()) {
    auto u = frontier.front(); frontier.pop();
    cout << "Frontier tip: " << u << endl;
    for (auto v : g.out_neigh(u)) {
      if (parent[v] == INVALID_NODE_ID) {
        parent[v] = u;
        frontier.push(v);
      }
    }
  }

  cout << "offset_accesses: " << offset_accesses << endl;
  cout << "neighs_accesses: " << neighs_accesses << endl;

  return parent;
}

void PrintBFSStats(const Graph &g, const pvector<NodeID> &bfs_tree) {
  int64_t tree_size = 0;
  int64_t n_edges = 0;
  for (NodeID n : g.vertices()) {
    if (bfs_tree[n] != INVALID_NODE_ID) {
      n_edges += g.out_degree(n);
      tree_size++;
    }
  }

  cout << "BFS Tree has " << tree_size << " nodes and ";
  cout << n_edges << " edges" << endl;
}

bool BFSVerifier(const Graph &g, NodeID source, const pvector<NodeID> &parent) {

  (void)g;

  // Get "route" to KTH
  cout << "KTH";
  NodeID next = 4325269;

  while(next != source) {
    next = parent[next];
    cout << "-> " << next;
  }

  cout << endl;

  return true;
}

int main(int argc, char *argv[]) {
  CLApp cli(argc, argv, "breadth-first search");
  if (!cli.ParseArgs())
    return -1;

  Builder b(cli);
  Graph g = b.MakeGraph();

  SourcePicker<Graph> sp(g, cli.start_vertex());
  auto BFSBound = [&sp](const Graph &g_) { return DOBFS(g_, sp.PickNext()); };

  SourcePicker<Graph> vsp(g, cli.start_vertex());
  auto VerifierBound = [&vsp](const Graph &g_, const pvector<NodeID> &parent) {
    return BFSVerifier(g_, vsp.PickNext(), parent);
  };

  BenchmarkKernel(cli, g, BFSBound, PrintBFSStats, VerifierBound);
  return 0;
}
