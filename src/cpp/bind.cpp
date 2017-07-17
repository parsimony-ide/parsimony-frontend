#include <emscripten/bind.h>
#include "inference.h"
#include "parser.h"

using namespace emscripten;

EMSCRIPTEN_BINDINGS(my_module) {

  /** parser.h **/

  class_<Grammar>("Grammar")
    .constructor<int, int>()
    .function("add", &Grammar::add)
    .function("print", &Grammar::print)
    ;

  class_<CYK>("CYK")
    .constructor<int, int, Grammar&>()
    .function("set_cyk", &CYK::set_cyk)
    .function("unset_cyk", &CYK::unset_cyk)
    .function("get_cyk", &CYK::get_cyk)
    .function("get_lmax", &CYK::get_lmax)
    .function("parse", &CYK::parse)
    .function("parse_partial", &CYK::parse_partial)
    .function("get_colors", &CYK::get_colors)
    .function("ignore", &CYK::ignore)
    .function("colorize", &CYK::colorize)
    .function("init_colorize_partial", &CYK::init_colorize_partial)
    .function("colorize_partial", &CYK::colorize_partial)
    .function("print_cyk", &CYK::print_cyk)
    .function("print_col", &CYK::print_col)
    .function("print_info", &CYK::print_info)
    ;

  class_<ColorSet>("ColorSet")
    .constructor<>()
    .function("nt", &ColorSet::nt)
    .function("i", &ColorSet::i)
    .function("l", &ColorSet::l)
    .function("size", &ColorSet::size)
    ;

  /** inference.h **/

  class_<VertexInfo>("VertexInfo")
    .constructor<>()
    .function("add_position", &VertexInfo::add_position)
    ;

  class_<Solution>("Solution")
    .constructor<>()
    .function("get_raws", &Solution::get_raws)
    .function("get_paths", &Solution::get_paths)
    .function("get_compressed_path", &Solution::get_compressed_path)
    .function("print", &Solution::print)
    ;

  class_<ConstraintState>("ConstraintState")
    .constructor<>()
    .function("add_provenance", &ConstraintState::add_provenance)
    .function("add_edge", &ConstraintState::add_edge)
    .function("add_edge_sym", &ConstraintState::add_edge_sym)
    .function("mark_as_terminal", &ConstraintState::mark_as_terminal)
    .function("solve_shortest", &ConstraintState::solve_shortest)
    .function("solve_shortest_non_unit", &ConstraintState::solve_shortest_non_unit)
    .function("empty", &ConstraintState::empty)
    .function("num_provenance_elements", &ConstraintState::num_provenance_elements)
    .function("get_provenance_sample_id", &ConstraintState::get_provenance_sample_id)
    .function("get_provenance_nt", &ConstraintState::get_provenance_nt)
    .function("get_provenance_i", &ConstraintState::get_provenance_i)
    .function("get_provenance_l", &ConstraintState::get_provenance_l)
    .function("get_edges", &ConstraintState::get_edges)
    .function("print", &ConstraintState::print)
    .class_function("intersect", &ConstraintState::intersect);
    ;

  register_vector<int>("VInt");
  register_vector<std::vector<int>>("VVInt");
  register_vector<std::vector<std::vector<int>>>("VVVInt");

}


