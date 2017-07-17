#ifndef _inference_h
#define _inference_h

#include <vector>
#include <set>
#include <map>
#include <memory>
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/breadth_first_search.hpp>
#include <boost/iterator/filter_iterator.hpp>

typedef int pos_t;
typedef int sym_t;

class VertexInfo {
  public:

    std::vector<pos_t> positions;

    VertexInfo();

    void add_position(pos_t position);
    bool operator<(VertexInfo const &right) const;
    void print();

    friend std::ostream & operator<<(std::ostream &stream, const VertexInfo &vi);

};

class EdgeInfo {
  public:

    std::vector<sym_t> syms;

    EdgeInfo();

    void add_sym(sym_t sym);

    friend std::ostream & operator<<(std::ostream &stream, const EdgeInfo &ei);

};

class ProvenanceElement {
  public:

    int sample_id;
    sym_t nt;
    pos_t i;
    int l;

    ProvenanceElement(int sample_id, sym_t nt, pos_t i, int l);

    void print();

    friend std::ostream & operator<<(std::ostream &stream, const ProvenanceElement &elem);

};

class Provenance {
  public:

    std::vector<ProvenanceElement> elems;

    Provenance();

    void add_provenance(int sample_id, sym_t nt, pos_t i, int l);
    void print();

    friend std::ostream & operator<<(std::ostream &stream, const Provenance &provenance);
};

typedef boost::adjacency_list<boost::setS, boost::vecS, boost::bidirectionalS, VertexInfo, EdgeInfo> Graph ;
typedef boost::graph_traits<Graph>::vertex_descriptor vertex_t;
typedef boost::graph_traits<Graph>::edge_descriptor edge_t;

typedef std::pair<vertex_t, vertex_t> vertex_pair_t;
typedef std::list<vertex_t> path_t;

class HasNoIncomingEdge {
  private:

    const Graph &g;

  public:

    HasNoIncomingEdge(const Graph &_g) : g(_g) {}

    bool operator()(const vertex_t &v) {
      Graph::in_edge_iterator it, end;
      std::tie(it, end) = boost::in_edges(v, g);
      return it == end;
    }
};

class Solution {
  public:

    typedef std::vector<std::vector<pos_t>> raw_t;
    typedef std::vector<std::vector<sym_t>> path_solution_t;

    std::vector<raw_t> raws;
    std::vector<path_solution_t> paths;
    path_solution_t compressed_path;

    Solution();

    void compress();
    void print();

    void get_raws(std::vector<raw_t> &output);
    void get_paths(std::vector<path_solution_t> &output);
    void get_compressed_path(path_solution_t &output);

    friend std::ostream & operator<<(std::ostream &stream, const Solution &solution);
};

class ConstraintState {
  private:

    Provenance provenance;
    Graph table;
    std::map<VertexInfo, vertex_t> vertex_map;
    std::set<sym_t> terminal_set;

    bool has_vertex(VertexInfo &vi);
    vertex_t _add_vertex(VertexInfo &vi);
    edge_t _add_edge(VertexInfo &from, VertexInfo &to);
    void compute_all_path_nodes(std::set<vertex_t> &path_nodes);

    typedef boost::filter_iterator<HasNoIncomingEdge,Graph::vertex_iterator> root_iterator;

    root_iterator root_begin() const;
    root_iterator root_end() const;

    // intersection
    static void init_node_pairs(ConstraintState &c1, ConstraintState &c2, std::set<vertex_pair_t> &node_pairs);
    static void print_node_pairs(ConstraintState &c1, ConstraintState &c2, std::set<vertex_pair_t> &node_pairs);
    static bool intersect_iterate(ConstraintState &c1, ConstraintState &c2, ConstraintState &dest, std::set<vertex_pair_t> &node_pairs);
    static void intersect_provenance(ConstraintState &c1, ConstraintState &c2, ConstraintState &dest);
    static void intersect_terminal_set(ConstraintState &c1, ConstraintState &c2, ConstraintState &dest);

    // shortest path
    std::vector<path_t> shortest_paths(Graph &g, vertex_t u, vertex_t v);
    std::vector<path_t> shortest_paths_iterate(Graph &g, vertex_t u, std::vector<path_t> &paths, bool &solution_found);

    // helpers
    void remove_unit_paths();
    void remove_non_solution_nodes();
    static bool has_vertex_descriptor(vertex_t u, Graph &g);

  public:

    ConstraintState();

    void add_provenance(int sample_id, sym_t nt, pos_t i, int l);
    void add_edge(VertexInfo &from, VertexInfo &to);
    void add_edge_sym(VertexInfo &from, VertexInfo &to, sym_t sym);
    void mark_as_terminal(sym_t sym);
    void solve_shortest(Solution &solution);
    void solve_shortest_non_unit(Solution &solution);
    bool empty();

    int num_provenance_elements();
    int get_provenance_sample_id(int n);
    sym_t get_provenance_nt(int n);
    pos_t get_provenance_i(int n);
    int get_provenance_l(int n);

    void get_edges(std::vector<std::vector<pos_t>> &sources,
        std::vector<std::vector<pos_t>> &targets,
        std::vector<std::vector<sym_t>> &syms);

    VertexInfo start_node();
    VertexInfo end_node();

    void print();

    friend std::ostream & operator<<(std::ostream &stream, const ConstraintState &constraint);

    static void intersect(ConstraintState &c1, ConstraintState &c2, ConstraintState &dest);
};

class BFSPredGraphVisitor : public boost::default_bfs_visitor {
  public:

    Graph &p;
    std::set<vertex_t> &active;

    BFSPredGraphVisitor(Graph &preds, std::set<vertex_t> &active_vertices);

    void examine_vertex(const vertex_t &s, const Graph &g) const;

};

#endif
