// Copyright (c) 2015, The Regents of the University of California (Regents)
// See LICENSE.txt for license details

#include <algorithm>
#include <iostream>
#include <vector>

#include "benchmark.h"
#include "builder.h"
#include "command_line.h"
#include "graph.h"
#include "pvector.h"

using namespace std;

/*
GAP Benchmark Suite
Kernel: PageRank (PR)
Author: Scott Beamer

Will return pagerank scores for all vertices once total change < epsilon

This PR implementation uses the traditional iterative approach. It perform
updates in the pull direction to remove the need for atomics, and it allows
new values to be immediately visible (like Gauss-Seidel method). The prior PR
implemention is still available in src/pr_spmv.cc.

emacs-lisp implementation embedded in the comments
Author: Konstantinos Sotiropoulos
*/

typedef float ScoreT;

// (setq kDamp 0.85)
const float kDamp = 0.85f;

// (find-file "/Users/ioanniss/cheatsheets/spzip_graph.png")
// (setq in-neighs [(1) (0 3) (0 1 3) (2)])
// (setq num-nodes (length in-neighs))
pvector<ScoreT> PageRankPullGS(const Graph &g, int max_iters, double epsilon = 0) {

  // (setq init-score (/ 1.0 num-nodes))
  const ScoreT init_score = 1.0f / static_cast<float>(g.num_nodes());

  // (setq base-score (/ (- 1 kDamp) num-nodes))
  const ScoreT base_score = (1.0f - kDamp) / static_cast<float>(g.num_nodes());

  // (setq scores [0 0 0 0])
  // (fillarray scores init-score)
  pvector<ScoreT> scores(g.num_nodes(), init_score);

  // (setq out-contrib (copy-sequence scores))
  pvector<ScoreT> outgoing_contrib(g.num_nodes());

  /*

    (setq out-degree [0 0 0 0])
    (dotimes (v num-nodes)
      (dolist (v (aref in-neighs v))
        (aset out-degree v (+ (aref out-degree v) 1))))

    (dotimes (v (length out-degree))
      (aset out-contrib v (/ (aref out-contrib v) (aref out-degree v))))
   */
  #pragma omp parallel for
  for (NodeID n=0; n < g.num_nodes(); n++)
    outgoing_contrib[n] = init_score / g.out_degree(n);

  for (int iter=0; iter < max_iters; iter++) {

    /*
      (setq err 0.0)
      (dotimes (u num-nodes)
        (let* ((incoming-total (seq-reduce
			                           (lambda (acc x) (+ acc (aref out-contrib x)))
                                 (aref in-neighs u) 0.0))
               (old-score (aref scores u))
	             (new-score (+ base-score (* kDamp incoming-total))))
          (progn
            (aset scores u new-score)
            (aset out-contrib u (/ new-score (aref out-degree u)))
            (setq err (+ err (abs (- new-score old-score)))))))
      err
     */
    double error = 0;
    #pragma omp parallel for reduction(+ : error) schedule(dynamic, 16384)
    for (NodeID u=0; u < g.num_nodes(); u++) {
      ScoreT incoming_total = 0;
      for (NodeID v : g.in_neigh(u))
        incoming_total += outgoing_contrib[v];

      ScoreT old_score = scores[u];
      scores[u] = base_score + kDamp * incoming_total;
      outgoing_contrib[u] = scores[u] / g.out_degree(u);

      error += static_cast<double>(fabs(scores[u] - old_score));
    }

    printf(" %2d    %lf\n", iter, error);
    if (error < epsilon)
      break;
  }

  return scores;
}

void PrintTopScores(const Graph &g, const pvector<ScoreT> &scores) {
  vector<pair<NodeID, ScoreT>> score_pairs(g.num_nodes());
  for (NodeID n=0; n < g.num_nodes(); n++) {
    score_pairs[n] = make_pair(n, scores[n]);
  }
  int k = 5;
  vector<pair<ScoreT, NodeID>> top_k = TopK(score_pairs, static_cast<size_t>(k));
  k = min(k, static_cast<int>(top_k.size()));
  for (auto kvp : top_k)
    cout << kvp.second << ":" << kvp.first << endl;
}

// Verifies by asserting a single serial iteration in push direction has
//   error < target_error
bool PRVerifier(const Graph &g, const pvector<ScoreT> &scores,
                        double target_error) {
  const ScoreT base_score = (1.0f - kDamp) / g.num_nodes();
  pvector<ScoreT> incomming_sums(g.num_nodes(), 0);
  double error = 0;
  for (NodeID u : g.vertices()) {
    ScoreT outgoing_contrib = scores[u] / g.out_degree(u);
    for (NodeID v : g.out_neigh(u))
      incomming_sums[v] += outgoing_contrib;
  }
  for (NodeID n : g.vertices()) {
    error += static_cast<double>(fabs(base_score + kDamp * incomming_sums[n] - scores[n]));
    incomming_sums[n] = 0;
  }
  PrintTime("Total Error", error);
  return error < target_error;
}


int main(int argc, char* argv[]) {
  CLPageRank cli(argc, argv, "pagerank", 1e-4, 20);
  if (!cli.ParseArgs())
    return -1;


  Builder b(cli);

  auto PRBound = [&cli] (const Graph &g) {
    return PageRankPullGS(g, cli.max_iters(), cli.tolerance());
  };
  auto VerifierBound = [&cli] (const Graph &g, const pvector<ScoreT> &scores) {
    return PRVerifier(g, scores, cli.tolerance());
  };

  Graph g = b.MakeGraph();
  BenchmarkKernel(cli, g, PRBound, PrintTopScores, VerifierBound);

  return 0;
}
