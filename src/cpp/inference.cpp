#include "inference.h"
#include "debug.h"
#include <algorithm>
#include <tuple>
#include <boost/graph/lookup_edge.hpp>

////////////////////////////////////////////////////////////////////////////////
// VertexInfo
////////////////////////////////////////////////////////////////////////////////

VertexInfo::VertexInfo() {}

void VertexInfo::add_position(pos_t position) {
  positions.push_back(position);
}

bool VertexInfo::operator<(VertexInfo const &right) const {
  std::vector<pos_t> right_positions = right.positions;
  size_t n = positions.size();
  for (size_t i = 0; i < n; ++i) {
    pos_t left = positions[i];
    pos_t right = right_positions[i];
    if (left < right) {
      return true;
    } else if (right < left) {
      return false;
    }
  }
  return false;
}

void VertexInfo::print() {
  std::cout << *this;
}

std::ostream & operator<<(std::ostream &stream, const VertexInfo &vi) {
  if (vi.positions.empty()) {
    return stream << "[]";
  }

  auto it = vi.positions.begin();
  stream << "[";
  stream << *it;
  ++it;
  for (; it != vi.positions.end(); ++it) {
    stream << " " << *it;
  }
  return stream << "]";
}

////////////////////////////////////////////////////////////////////////////////
// EdgeInfo
////////////////////////////////////////////////////////////////////////////////

EdgeInfo::EdgeInfo() {}

void EdgeInfo::add_sym(sym_t sym) {
  syms.push_back(sym);
}

std::ostream & operator<<(std::ostream &stream, const EdgeInfo &ei) {
  if (ei.syms.empty()) {
    return stream << "#{}";
  }

  auto it = ei.syms.begin();
  stream << "#{";
  stream << *it;
  ++it;
  for (; it != ei.syms.end(); ++it) {
    stream << " " << *it;
  }
  return stream << "}";
}

////////////////////////////////////////////////////////////////////////////////
// ProvenanceElement
////////////////////////////////////////////////////////////////////////////////

ProvenanceElement::ProvenanceElement(int sample_id, sym_t nt, pos_t i, int l)
  : sample_id(sample_id), nt(nt), i(i), l(l) {}


void ProvenanceElement::print() {
  std::cout << *this;
}

std::ostream & operator<<(std::ostream &stream, const ProvenanceElement &elem) {
  return stream << "[" << elem.sample_id << " [" << elem.nt << " " << elem.i << " " << elem.l << "]]";
}

////////////////////////////////////////////////////////////////////////////////
// Provenance
////////////////////////////////////////////////////////////////////////////////

Provenance::Provenance() {}

void Provenance::add_provenance(int sample_id, sym_t nt, pos_t i, int l) {
  elems.push_back(ProvenanceElement(sample_id, nt, i, l));
}

void Provenance::print() {
  std::cout << *this;
}

std::ostream & operator<<(std::ostream &stream, const Provenance &provenance) {
  stream << "[";
  for (auto it=provenance.elems.begin(); it < provenance.elems.end(); ++it) {
    stream << " " << *it;
  }
  stream << " ]";
  return stream;
}

////////////////////////////////////////////////////////////////////////////////
// ConstraintState
////////////////////////////////////////////////////////////////////////////////

ConstraintState::ConstraintState() {}

void ConstraintState::add_provenance(int sample_id, sym_t nt, pos_t i, int l) {
  DEBUG_PRINT(("add_provenance : sample_id=%d nt=%d i=%d l=%d\n", sample_id, nt, i, l));
  provenance.add_provenance(sample_id, nt, i, l);
};

bool ConstraintState::has_vertex(VertexInfo &vi) {
  return vertex_map.find(vi) != vertex_map.end();
}

bool ConstraintState::has_vertex_descriptor(vertex_t u, Graph &g) {
  Graph::vertex_iterator it, end;
  std::tie(it, end) = boost::vertices(g);
  for (; it != end; ++it) {
    if (*it == u) {
      return true;
    }
  }
  return false;
}

vertex_t ConstraintState::_add_vertex(VertexInfo &vi) {
  dout << "add_vertex : " << vi << std::endl;
  if (has_vertex(vi)) {
    dout << "  vertex already exists" << std::endl;
    return vertex_map[vi];
  }

  vertex_t v = boost::add_vertex(table);
  vertex_map[vi] = v;
  table[v].positions = vi.positions;
  return v;
}

edge_t ConstraintState::_add_edge(VertexInfo &from, VertexInfo &to) {
  dout << "add_edge : from=" << from << " to=" << to << std::endl;
  vertex_t u = _add_vertex(from);
  vertex_t v = _add_vertex(to);
  edge_t e;
  bool _;
  std::tie(e,_) = boost::add_edge(u, v, table);
  return e;
}

void ConstraintState::add_edge(VertexInfo &from, VertexInfo &to) {
  _add_edge(from, to);
}


void ConstraintState::add_edge_sym(VertexInfo &from, VertexInfo &to, sym_t sym) {
  dout << "add_edge_sym : from=" << from << " to=" << to << " sym=" << sym << std::endl;
  if (has_vertex(from) && has_vertex(to)) {
    vertex_t u = vertex_map[from];
    vertex_t v = vertex_map[to];
    edge_t e;
    bool _;
    std::tie(e, _) = boost::add_edge(u, v, table);
    table[e].add_sym(sym);
    // syms must be in sorted order for set intersection to work
    std::sort(table[e].syms.begin(), table[e].syms.end());
  }
}

void ConstraintState::mark_as_terminal(sym_t sym) {
  dout << "mark_as_terminal " << sym << std::endl;
  terminal_set.insert(sym);
}

VertexInfo ConstraintState::start_node() {
  VertexInfo vi;
  for (auto elem : provenance.elems) {
    vi.add_position(elem.i);
  }
  dout << "start_node = " << vi << std::endl;
  return vi;
}

VertexInfo ConstraintState::end_node() {
  VertexInfo vi;
  for (auto elem : provenance.elems) {
    vi.add_position(elem.i + elem.l);
  }
  dout << "end_node = " << vi << std::endl;
  return vi;
}

ConstraintState::root_iterator ConstraintState::root_begin() const {
  Graph::vertex_iterator begin, end;
  std::tie(begin, end) = boost::vertices(table);
  HasNoIncomingEdge pred(table);
  return make_filter_iterator(pred, begin, end);
}

ConstraintState::root_iterator ConstraintState::root_end() const {
  Graph::vertex_iterator _, end;
  std::tie(_, end) = boost::vertices(table);
  HasNoIncomingEdge pred(table);
  return make_filter_iterator(pred, end, end);
}

void ConstraintState::print() {
  std::cout << *this;
}

std::ostream & operator<<(std::ostream &stream, const ConstraintState &constraint) {

  stream << "constraint state " <<
    "(" <<
    boost::num_vertices(constraint.table) << " vertices, " <<
    boost::num_edges(constraint.table) << " edges, " <<
    "provenance = " << constraint.provenance <<
    ") : " <<
    std::endl;

  Graph::edge_iterator it, end;
  Graph g = constraint.table;
  std::tie(it, end) = boost::edges(g);
  for (; it != end; ++it) {
    edge_t e = *it;
    vertex_t u = boost::source(e, g);
    vertex_t v = boost::target(e, g);
    stream << "    | " << g[u] << " -> " << g[v] << " : " << g[e] << std::endl;
  }

  // stream << "roots =";
  // ConstraintState::root_iterator rit = constraint.root_begin();
  // ConstraintState::root_iterator rend = constraint.root_end();
  // for (; rit != rend; ++rit) {
  //   vertex_t v = *rit;
  //   stream << " " << g[v];
  // }
  // stream << std::endl;

  return stream;
}

//------------------------------------------------------------------------------
// Intersect
//------------------------------------------------------------------------------

void ConstraintState::init_node_pairs(ConstraintState &c1, ConstraintState &c2, std::set<vertex_pair_t> &node_pairs) {
  for (auto r1 = c1.root_begin(); r1 != c1.root_end(); ++r1) {
    vertex_t u = *r1;
    for (auto r2 = c2.root_begin(); r2 != c2.root_end(); ++r2) {
      vertex_t v = *r2;
      node_pairs.insert(std::make_pair(u, v));
    }
  }
}

void ConstraintState::print_node_pairs(ConstraintState &c1, ConstraintState &c2, std::set<vertex_pair_t> &node_pairs) {
  dout << "node_pairs = " << std::endl;
  for (auto it = node_pairs.begin(); it != node_pairs.end(); ++it) {
    vertex_t u, v;
    std::tie(u, v) = *it;
    dout << "  " << c1.table[u] << " " << c2.table[v] << std::endl;
  }
}

bool ConstraintState::intersect_iterate(ConstraintState &c1, ConstraintState &c2, ConstraintState &dest, std::set<vertex_pair_t> &node_pairs) {
  dout << "intersect_iterate" << std::endl;

  if (node_pairs.empty()) {
    dout << "no more node_pairs " << std::endl;
    return false;
  }

  auto it = node_pairs.begin();
  vertex_t u, v;
  std::tie(u, v) = *it;
  node_pairs.erase(it);
  dout << "current pair = " << c1.table[u] << " " << c2.table[v] << std::endl;

  Graph::out_edge_iterator u_out_it, u_out_end, v_out_it, v_out_end;

  for (std::tie(u_out_it, u_out_end) = boost::out_edges(u, c1.table); u_out_it != u_out_end; ++u_out_it) {
    edge_t e1 = *u_out_it;
    std::vector<sym_t> &e1_syms = c1.table[e1].syms;
    vertex_t usucc = boost::target(e1, c1.table);

    for (std::tie(v_out_it, v_out_end) = boost::out_edges(v, c2.table); v_out_it != v_out_end; ++v_out_it) {
      edge_t e2 = *v_out_it;
      std::vector<sym_t> &e2_syms = c2.table[e2].syms;
      vertex_t vsucc = boost::target(e2, c2.table);

      dout << "succ pair candidate " <<
        usucc << " " << c1.table[usucc] << " " <<
        vsucc << " " << c2.table[vsucc] << " " << std::endl;

      std::set<sym_t> sym_intersection;
      std::set_intersection(e1_syms.begin(), e1_syms.end(), e2_syms.begin(), e2_syms.end(),
          std::inserter(sym_intersection, sym_intersection.begin()));

      if (sym_intersection.empty()) {
        dout << "empty intersection " <<
          u << " " << c1.table[u] << " " <<
          usucc << " " << c1.table[usucc] << " " <<
          e1 << " " << c1.table[e1] << " || " <<
          v << " " << c2.table[v] << " " <<
          vsucc << " " << c2.table[vsucc] << " " <<
          e2 << " " << c2.table[e2] << " " << std::endl;
        continue;
      }

      VertexInfo from, to;
      // take all positions in u and v and place them in from
      for (auto &pos : c1.table[u].positions) {
        from.add_position(pos);
      }
      for (auto &pos : c2.table[v].positions) {
        from.add_position(pos);
      }
      // take all positions in usucc and vsucc and place them in to
      for (auto &pos : c1.table[usucc].positions) {
        to.add_position(pos);
      }
      for (auto &pos : c2.table[vsucc].positions) {
        to.add_position(pos);
      }

      edge_t e3 = dest._add_edge(from, to);
      EdgeInfo &ei = dest.table[e3];
      for (auto sym : sym_intersection) {
        ei.add_sym(sym);
      }

      dout << "add edge " <<
        from << " " <<
        to   << " " <<
        ei << std::endl;

      node_pairs.insert(std::make_pair(usucc, vsucc));
    }
  }

  return true;
}

void ConstraintState::intersect_provenance(ConstraintState &c1, ConstraintState &c2, ConstraintState &dest) {
  for (auto elem : c1.provenance.elems) {
    dest.provenance.elems.push_back(elem);
  }
  for (auto elem : c2.provenance.elems) {
    dest.provenance.elems.push_back(elem);
  }
}

void ConstraintState::intersect_terminal_set(ConstraintState &c1, ConstraintState &c2, ConstraintState &dest) {
  for (auto sym : c1.terminal_set) {
    dest.terminal_set.insert(sym);
  }
  for (auto sym : c2.terminal_set) {
    dest.terminal_set.insert(sym);
  }
}

void ConstraintState::intersect(ConstraintState &c1, ConstraintState &c2, ConstraintState &dest) {
  dout << "intersect : " << std::endl;
  dout << "  constraint 1 : " << std::endl << c1 << std::endl;
  dout << "  constraint 2 : " << std::endl << c2 << std::endl;

  std::set<vertex_pair_t> node_pairs;
  init_node_pairs(c1, c2, node_pairs);

  print_node_pairs(c1, c2, node_pairs);
  while(intersect_iterate(c1, c2, dest, node_pairs)) {
    print_node_pairs(c1, c2, node_pairs);
  }

  intersect_provenance(c1, c2, dest);
  intersect_terminal_set(c1, c2, dest);

  dout << "pre-remove =" << std::endl;
  dout << dest;

  dest.remove_non_solution_nodes();

  dout << "post-remove =" << std::endl;
  dout << dest;
}

void ConstraintState::compute_all_path_nodes(std::set<vertex_t> &path_nodes) {
  dout << "compute_all_path_nodes" << std::endl;

  VertexInfo from = start_node();
  VertexInfo to = end_node();

  if (!has_vertex(from) || !has_vertex(to)) {
    dout << "either source or target does not exist" << std::endl;
    return;
  }

  vertex_t u = vertex_map[from];
  vertex_t v = vertex_map[to];

  Graph preds;
  std::set<vertex_t> active;
  dout << "preds begin" << std::endl;
  dout << "vertices = " << boost::num_vertices(preds) << std::endl;
  dout << "edges    = " << boost::num_edges(preds) << std::endl;

  BFSPredGraphVisitor visitor(preds, active);
  breadth_first_search(table, u, boost::visitor(visitor));

  dout << "preds end" << std::endl;
  dout << "vertices = " << boost::num_vertices(preds) << std::endl;
  dout << "edges    = " << boost::num_edges(preds) << std::endl;

  if (!has_vertex_descriptor(v, preds)) {
    dout << "preds does not reach v" << std::endl;
    return;
  }

  Graph preds2;
  std::set<vertex_t> active2;
  dout << "preds2 begin" << std::endl;
  dout << "vertices = " << boost::num_vertices(preds2) << std::endl;
  dout << "edges    = " << boost::num_edges(preds2) << std::endl;

  BFSPredGraphVisitor visitor2(preds2, active2);
  breadth_first_search(preds, v, boost::visitor(visitor2));

  dout << "preds2 end" << std::endl;
  dout << "vertices = " << boost::num_vertices(preds2) << std::endl;
  dout << "edges    = " << boost::num_edges(preds2) << std::endl;

  Graph::vertex_iterator it, end;
  std::tie(it, end) = boost::vertices(preds2);
  for (auto v : active2) {
    path_nodes.insert(v);
  }
}

void ConstraintState::remove_unit_paths() {
  dout << "remove_unit_paths" << std::endl;

  VertexInfo from = start_node();
  VertexInfo to = end_node();

  if (!has_vertex(from) || !has_vertex(to)) {
    dout << "either source or target does not exist" << std::endl;
    return;
  }

  vertex_t u = vertex_map[from];
  vertex_t v = vertex_map[to];

  edge_t e;
  bool found;
  std::tie(e, found) = boost::lookup_edge(u, v, table);

  if (found) {
    std::vector<sym_t> syms(table[e].syms.begin(), table[e].syms.end());

    auto terminal_set = this->terminal_set;
    auto is_nonterminal = [terminal_set](sym_t sym){ return terminal_set.find(sym) == terminal_set.end(); };
    syms.erase(std::remove_if(syms.begin(), syms.end(), is_nonterminal), syms.end());

    if (syms.size() == 0) {
      dout << "unit edge found, removing " << from << " " << to << std::endl;
      boost::remove_edge(e, table);
      if (0 == boost::degree(u, table)) {
        dout << "removing start node " << table[u] << std::endl;
        vertex_map.erase(vertex_map.find(table[u]));
        boost::clear_vertex(u, table);
      }
      if (0 == boost::degree(v, table)) {
        dout << "removing end node " << table[v] << std::endl;
        vertex_map.erase(vertex_map.find(table[v]));
        boost::clear_vertex(v, table);
      }
    }
  }
}

void ConstraintState::remove_non_solution_nodes() {
  dout << "remove_non_solution_nodes" << std::endl;
  std::set<vertex_t> path_nodes;
  compute_all_path_nodes(path_nodes);

  dout << "path nodes = " << std::endl;
  for (auto vertex : path_nodes) {
    dout << "  " << vertex << " " << table[vertex] << std::endl;
  }

  dout << "graph nodes = " << std::endl;
  std::set<vertex_t> non_path_nodes;
  Graph::vertex_iterator it, end;
  std::tie(it, end) = boost::vertices(table);
  for (; it != end; ++it) {
    vertex_t v = *it;
    dout << "  " << v << " " << table[v] << std::endl;
    if (path_nodes.find(v) == path_nodes.end()) {
      non_path_nodes.insert(v);
    }
  }

  dout << "non-path-nodes = " << std::endl;
  for (auto v : non_path_nodes) {
    dout << table[v] << std::endl;
    vertex_map.erase(vertex_map.find(table[v]));
    boost::clear_vertex(v, table);
  }
}

//------------------------------------------------------------------------------
// Shortest Paths
//------------------------------------------------------------------------------

std::vector<path_t> ConstraintState::shortest_paths_iterate(Graph &g,
    vertex_t u,
    std::vector<path_t> &paths,
    bool &solution_found) {
  dout << "shortest_paths_iterate" << std::endl;
  if (paths.empty()) {
    return paths;
  }

  std::vector<path_t> new_paths;
  for (auto path : paths) {
    vertex_t n = path.front();

    dout << "  current = " << table[n] << std::endl;

    Graph::in_edge_iterator it, end;
    std::tie(it, end) = boost::in_edges(n, g);
    // extend paths by predecessor
    for (; it != end; ++it) {
      vertex_t pred = boost::source(*it, g);

      dout << "  pred = " << table[pred] << std::endl;

      path_t new_path(path.begin(), path.end());
      new_path.push_front(pred);

      dout << "  new_path = ";
      for (auto elem : new_path) {
        dout << table[elem] << " ";
      }
      dout << std::endl;

      new_paths.push_back(new_path);
      if (pred == u) {
        dout << "  solution found" << std::endl;
        solution_found = true;
      }
    }
  }

  if (solution_found) {
    auto is_not_solution = [u](path_t path){ return path.front() != u; };
    new_paths.erase(std::remove_if(new_paths.begin(), new_paths.end(), is_not_solution), new_paths.end());
  }

  return new_paths;
}

std::vector<path_t> ConstraintState::shortest_paths(Graph &g, vertex_t u, vertex_t v) {
  dout << "shortest_paths" << std::endl;

  std::vector<path_t> paths;
  paths.push_back({v});
  bool solution_found = false;

  int limit = 100;
  while (!solution_found) {
    paths = shortest_paths_iterate(g, u, paths, solution_found);

    if (solution_found) {
      // dout << "solution found" << std::endl;
      return paths;
    }

    if (paths.empty()) {
      dout << "no solutions" << std::endl;
      return paths;
    }

    if (limit <= 0) {
      dout << "limit reached" << std::endl;
      return paths;
    }

    --limit;
  }

  // should be unreachable
  return paths;
}

void ConstraintState::solve_shortest(Solution &solution) {
  dout << "solve_shortest" << std::endl;

  VertexInfo from = start_node();
  VertexInfo to = end_node();

  if (!has_vertex(from) || !has_vertex(to)) {
    dout << "either source or target does not exist" << std::endl;
    return;
  }

  vertex_t u = vertex_map[from];
  vertex_t v = vertex_map[to];

  dout << "preds begin" << std::endl;

  Graph preds;
  std::set<vertex_t> active;
  BFSPredGraphVisitor visitor(preds, active);
  breadth_first_search(table, u, boost::visitor(visitor));

  dout << "preds end" << std::endl;

  if (!has_vertex_descriptor(v, preds)) {
    dout << "preds does not reach v" << std::endl;
    return;
  }

  dout << "preds2 begin" << std::endl;

  Graph preds2;
  std::set<vertex_t> active2;
  BFSPredGraphVisitor visitor2(preds2, active2);
  breadth_first_search(preds, v, boost::visitor(visitor2));

  dout << "preds2 end" << std::endl;

  auto paths = shortest_paths(preds2, u, v);

  dout << "num solution paths = " << paths.size() << std::endl;
  for (auto i = paths.begin(); i != paths.end(); ++i) {
    auto path = *i;
    dout << "  path (" << path.size() <<  " vertices)" << std::endl;

    Solution::raw_t raw;
    for (auto j = path.begin(); j != path.end(); ++j) {
      vertex_t v = *j;
      VertexInfo vi = table[v];
      raw.push_back(vi.positions);
    }
    solution.raws.push_back(raw);

    Solution::path_solution_t path_solution;
    for (auto j = path.begin(); j != path.end(); ++j) {
      auto k = j;
      ++k;

      if (k == path.end()) {
        continue;
      }
      dout << "    | " << table[*j] << " -> " << table[*k] << " : ";

      edge_t e;
      bool found;
      std::tie(e, found) = boost::lookup_edge(*j, *k, table);
      if (!found) {
        dout << "  should not happen : " << *j << " " << *k << std::endl;
        continue;
      }

      auto edge_info = table[e];
      dout << edge_info << std::endl;;

      path_solution.push_back(edge_info.syms);
    }
    solution.paths.push_back(path_solution);
  }
  solution.compress();
}

void ConstraintState::solve_shortest_non_unit(Solution &solution) {
  dout << "solve_shortest_non_unit" << std::endl;

  dout << "before remove : " << std::endl;
  dout << *this;

  remove_unit_paths();

  dout << "after remove : " << std::endl;
  dout << *this;

  solve_shortest(solution);
}

bool ConstraintState::empty() {
  bool result = boost::num_edges(table) == 0;
  dout << "empty = " << result << std::endl;
  return result;
}

int ConstraintState::num_provenance_elements() {
  return provenance.elems.size();
}

int ConstraintState::get_provenance_sample_id(int n) {
  return provenance.elems[n].sample_id;
}

sym_t ConstraintState::get_provenance_nt(int n) {
  return provenance.elems[n].nt;
}

pos_t ConstraintState::get_provenance_i(int n) {
  return provenance.elems[n].i;
}

int ConstraintState::get_provenance_l(int n) {
  return provenance.elems[n].l;
}

void ConstraintState::get_edges(std::vector<std::vector<pos_t>> &sources,
    std::vector<std::vector<pos_t>> &targets,
    std::vector<std::vector<sym_t>> &syms) {
  Graph::edge_iterator it, end;
  std::tie(it, end) = boost::edges(table);
  for (; it != end; ++it) {
    auto e = *it;
    auto source = boost::source(e, table);
    auto target = boost::target(e, table);
    sources.push_back(table[source].positions);
    targets.push_back(table[target].positions);
    syms.push_back(table[e].syms);
  }
}

////////////////////////////////////////////////////////////////////////////////
// Solution
////////////////////////////////////////////////////////////////////////////////

Solution::Solution() {}

void Solution::compress() {
  dout << "compressing " << paths.size() << " paths" << std::endl;

  if (paths.size() == 0) {
    return;
  }

  auto first_path = paths.front();
  std::vector<std::set<sym_t>> staging;
  for (auto it = first_path.begin(); it != first_path.end(); ++it) {
    staging.push_back(std::set<sym_t>());
  }

  for (auto it = paths.begin(); it != paths.end(); ++it) {
    auto path = *it;
    int j = 0;
    for (auto jt = path.begin(); jt != path.end(); ++jt, ++j) {
      auto syms = *jt;
      staging[j].insert(syms.begin(), syms.end());
    }
  }

  compressed_path.clear();
  for (auto it = staging.begin(); it != staging.end(); ++it) {
    auto node = *it;
    std::vector<sym_t> syms(node.begin(), node.end());
    compressed_path.push_back(syms);
  }
}

void Solution::get_raws(std::vector<raw_t> &output) {
  output = raws;
}

void Solution::get_paths(std::vector<path_solution_t> &output) {
  output = paths;
}

void Solution::get_compressed_path(path_solution_t &output) {
  output = compressed_path;
}

std::ostream & operator<<(std::ostream &stream, const Solution &solution) {
  stream << "solution (" << solution.paths.size() << " paths) : " << std::endl;
  for (auto path : solution.paths) {
    stream << "  ";
    for (auto syms : path) {
      stream << "#{";
      for (auto sym : syms) {
        stream << " " << sym;
      }
      stream << " } ";
    }
    stream << std::endl;
  }
  return stream;
}

void Solution::print() {
  std::cout << *this << std::endl;
}

////////////////////////////////////////////////////////////////////////////////
// BFSPredGraphVisitor
////////////////////////////////////////////////////////////////////////////////

BFSPredGraphVisitor::BFSPredGraphVisitor(Graph &preds, std::set<vertex_t> &active_vertices) : p(preds),
  active(active_vertices) {}

void BFSPredGraphVisitor::examine_vertex(const vertex_t &s, const Graph &g) const {
  dout << "examine_vertex: " << s << " " << g[s] << std::endl;
  Graph::out_edge_iterator it, end;
  std::tie(it, end) = boost::out_edges(s, g);

  for (; it != end; ++it) {
    vertex_t u = boost::target(*it, g);
    dout << "  succ: " << u << " " << g[u] << std::endl;
    dout << "  add_edge " << u << " -> " << s << std::endl;
    dout << "  num_vertices before add " << boost::num_vertices(p) << std::endl;
    dout << "  num_active before add " << active.size() << std::endl;
    boost::add_edge(u, s, p);
    active.insert(u);
    active.insert(s);
    dout << "  num_vertices after add " << boost::num_vertices(p) << std::endl;
    dout << "  num_active after add " << active.size() << std::endl;
  }
}

