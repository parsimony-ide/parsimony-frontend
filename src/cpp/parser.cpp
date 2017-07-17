#include "parser.h"
#include "debug.h"
#include <algorithm>

////////////////////////////////////////////////////////////////////////////////
// Dynamic Matrices
////////////////////////////////////////////////////////////////////////////////

template<typename T>
T** table2d(int n, int m) {
  T **table = new T*[n];
  for (int i = 0; i < n; ++i) {
    table[i] = new T[m]();
  }
  return table;
}

template<typename T>
void delete_table2d(T **table, int n) {
  for (int i = 0; i < n; ++i) {
    delete [] table[i];
  }
  delete[] table;
}


template<typename T>
T*** table3d(int n, int m, int l) {
  T ***table = new T**[n];
  for (int i = 0; i < n; ++i) {
    table[i] = new T*[m];
    for (int j = 0; j < m; ++j) {
      table[i][j] = new T[l]();
    }
  }
  return table;
}

template<typename T>
void delete_table3d(T ***table, int n, int m) {
  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < m; ++j) {
      delete[] table[i][j];
    }
    delete [] table[i];
  }
  delete[] table;
}

////////////////////////////////////////////////////////////////////////////////
// Grammar
////////////////////////////////////////////////////////////////////////////////

Grammar::Grammar(int n, int m): n(n), m(m) {
  table = table3d<int>(n, m, 3);
}

Grammar::~Grammar() {
  delete_table3d<int>(table, n, m);
}

void Grammar::add(int l, int r1, int r2) {
  int **row = table[l];
  int empty_col = 0;
  for (int i = 0; i < m; i++) {
    if (row[i][0] == 0) {
      empty_col = i;
      break;
    }
  }
  row[empty_col][0] = l;
  row[empty_col][1] = r1;
  row[empty_col][2] = r2;
}

int** Grammar::productions_with_lhs(int l) {
  return table[l];
}

void Grammar::print() {
  printf("Printing grammar [n = %d, m = %d]\n", n, m);
  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < m; ++j) {
      int *rule = table[i][j];
      if (rule[0] != 0) {
        printf("%d => %d %d\n", rule[0], rule[1], rule[2]);
      } else {
        break;
      }
    }
  }
}

////////////////////////////////////////////////////////////////////////////////
// ColorSet
////////////////////////////////////////////////////////////////////////////////

ColorSet::ColorSet() {}

void ColorSet::add(int _nt, int _i, int _l) {
  bool not_found = true;

  for (int i = 0; i < nts.size(); ++i) {
    if (nts[i] == _nt && is[i] == _i && ls[i] == _l) {
      DEBUG_PRINT(("already exists\n"));
      return;
    }
  }

  nts.push_back(_nt);
  is.push_back(_i);
  ls.push_back(_l);
}

int ColorSet::nt(int i) {
  return nts[i];
}

int ColorSet::i(int j) {
  return is[j];
}

int ColorSet::l(int i) {
  return ls[i];
}

size_t ColorSet::size() {
  return nts.size();
}

////////////////////////////////////////////////////////////////////////////////
// Score
////////////////////////////////////////////////////////////////////////////////

Score::Score() : coverage(0), largest(0), num(0) {}
Score::Score(int _coverage, int _largest, int _num) : coverage(_coverage), largest(_largest), num(_num) {}

bool Score::better_than(Score &other) {
  if (coverage > other.coverage) {
    return true;
  } else if (coverage < other.coverage) {
    return false;
  } else if (largest > other.largest) {
    return true;
  } else if (largest < other.largest) {
    return false;
  } else if (num > other.num) {
    return true;
  } else {
    return false;
  }
}

bool Score::equals(Score &other) {
  return coverage == other.coverage && largest == other.largest && num == other.num;
}

std::ostream & operator<<(std::ostream &strm, const Score &s) {
  return strm << "Score(" << s.coverage << "," << s.largest << "," << s.num << ")";
}

////////////////////////////////////////////////////////////////////////////////
// CYK
////////////////////////////////////////////////////////////////////////////////

CYK::CYK(int n, int m, Grammar &g): n(n), m(m), grammar(g) {
  lmax = 1+m;
  cyk_table = table3d<bool>(n, m, lmax);
  col_table = table2d<ColorSet>(m, lmax);
  score_table = table2d<Score>(m, lmax);
}

CYK::~CYK() {
  delete_table3d<bool>(cyk_table, n, m);
  delete_table2d<ColorSet>(col_table, m);
  delete_table2d<Score>(score_table, m);

  // The following is kind of unsafe: deleting the grammar even though it was initialized
  // in another scope. We do this because we know that no other reference to the grammar
  // escapes from asm.parser/cpp-init-cyk, so this instance holds the only reference
  // to the grammar.
  delete &grammar;
}

void CYK::set_cyk(int nt, int i, int l) {
  cyk_table[nt][i][l] = true;
}

void CYK::unset_cyk(int nt, int i, int l) {
  cyk_table[nt][i][l] = false;
}

bool CYK::get_cyk(int nt, int i, int l) {
  return cyk_table[nt][i][l];
}

int CYK::get_lmax() {
  return lmax;
}

/* Perform a complete parse. This may block for a long time. */
void CYK::parse() {
  for (int l = 2; l < lmax; ++l) {
    for (int nt = 1; nt < n; ++nt) {
      for (int i = 0; i <= m-l; ++i) {
        cyk_table[nt][i][l] = match(nt, i, l);
      }
    }
  }
}

/* Parse for only some values of l, then return the next value of l to use. The purpose of this method is for use in
   interactive environments where a long-running blocking call can cause the interface to become unresponsive. Instead,
   the caller must call parse_partial for each value of l, redraw, then call parse_partial on the next value of l, and
   so on until all values of l have been covered, which is indicated by a return value of 0. */
int CYK::parse_partial(int l) {
  int next_l = l + 10;
  for (; l < lmax && l < next_l; ++l) {
    for (int nt = 1; nt < n; ++nt) {
      for (int i = 0; i <= m-l; ++i) {
        cyk_table[nt][i][l] = match(nt, i, l);
      }
    }
  }
  if (l == lmax) {
    return 0;
  } else {
    return next_l;
  }
}

bool CYK::match(int nt, int i, int l) {
  // an initially true value is treated specially to mean "this entry is pinned to false"
  if (cyk_table[nt][i][l]) {
    return false;
  }

  int **ps = grammar.productions_with_lhs(nt);
  for (int j = 0; j < grammar.m; ++j) {
    int *p = ps[j];
    // check for end sentinel
    if (p[0] == 0) {
      break;
    }

    // this is a binary rule nt -> a b
    int a = p[1];
    int b = p[2];
    for (int k = 1; k < l; ++k) {
      bool prefix = cyk_table[a][i][k];
      bool suffix = cyk_table[b][i+k][l-k];
      bool match = prefix && suffix;
      if (match) {
        return true;
      }
    }
  }
  return false;
}

ColorSet & CYK::get_colors(int i, int l) {
  return col_table[i][l];
}

void CYK::set_color(int i, int l, int nt, int ci, int cl) {
  DEBUG_PRINT(("%20s i=%d l=%d [%d %d %d]\n", "set_color", i, l, nt, ci, cl));
  col_table[i][l].add(nt, ci, cl);
}

void CYK::set_colors(int i, int l, ColorSet &colors) {
  DEBUG_PRINT(("%20s i=%d l=%d num=%d\n", "set_colors", i, l, colors.size()));
  for (int j = 0; j < colors.size(); ++j) {
    set_color(i, l, colors.nt(j), colors.i(j), colors.l(j));
  }
}

void CYK::ignore(int nt) {
  ignored.insert(nt);
}

bool CYK::is_ignored(int nt) {
  return ignored.find(nt) != ignored.end();
}

Score & CYK::get_score(int i, int l) {
  return score_table[i][l];
}

void CYK::set_score(int i, int l, int coverage, int largest, int num) {
  score_table[i][l].coverage = coverage;
  score_table[i][l].largest = largest;
  score_table[i][l].num = num;
}

void CYK::compute_color(int i, int l) {

  DEBUG_PRINT(("compute_color i=%d l=%d\n", i, l));

  // compute set of non-ignored nts that cover entire range
  std::vector<int> nts;
  for (int nt = 1; nt < n; ++nt) {
    if (!is_ignored(nt) && get_cyk(nt, i, l)) {
      nts.push_back(nt);
    }
  }

  if (nts.size() > 0) {
    DEBUG_PRINT(("full %d %d\n", i, l));
    // found a new larger color that spans the entire range
    for (std::vector<int>::const_iterator nti = nts.begin(); nti != nts.end(); ++nti) {
      set_color(i, l, *nti, i, l);
      set_score(i, l, l, l, -1);
    }
  } else {
    DEBUG_PRINT(("partial %d %d\n", i, l));
    // otherwise, search for the best coloring amongst left/right groups
    Score score_so_far(0,0,-1000000);
    std::vector<ColorSet*> best;
    for (int k = 1; k < l; ++k) {
      DEBUG_PRINT(("score k=%d ", k));
      Score combined;
      ColorSet &lcolors  = get_colors(i, k);
      ColorSet &rcolors  = get_colors(i+k, l-k);
      Score &lscore      = get_score(i, k);
      Score &rscore      = get_score(i+k, l-k);
      combined.coverage  = lscore.coverage + rscore.coverage;
      combined.largest   = lscore.largest > rscore.largest ? lscore.largest : rscore.largest;
      combined.num       = lscore.num + rscore.num ;

      if (combined.better_than(score_so_far)) {
        DEBUG_PRINT(("better"));
        score_so_far = combined;
        best.clear();
        if (lcolors.size() > 0)
          best.push_back(&lcolors);
        if (rcolors.size() > 0)
          best.push_back(&rcolors);
      } else if (combined.equals(score_so_far)) {
        DEBUG_PRINT(("same"));
        if (lcolors.size() > 0)
          best.push_back(&lcolors);
        if (rcolors.size() > 0)
          best.push_back(&rcolors);
      } else {
        DEBUG_PRINT(("worse"));
      }
      dout << " " << combined;
      DEBUG_PRINT(("\n"));
    }

    for (std::vector<ColorSet*>::const_iterator I = best.begin(); I != best.end(); ++I) {
      set_colors(i, l, **I);
    }

    set_score(i, l, score_so_far.coverage, score_so_far.largest, score_so_far.num);
  }
}

void CYK::colorize() {
  // populate color_table and score_table with initial sets for l = 1
  DEBUG_PRINT(("init\n"));
  for (int i = 0; i < m; ++i) {
    for (int nt = 1; nt < n; ++nt) {
      if (!is_ignored(nt) && get_cyk(nt, i, 1)) {
        set_color(i, 1, nt, i, 1);
        set_score(i, 1, 1, 1, -1);
      }
    }
  }

  // now compute colors for spans with l > 1
  DEBUG_PRINT(("colorize\n"));
  for (int l = 2; l < lmax; ++l) {
    for (int i = 0; i <= m-l; ++i) {
      compute_color(i, l);
    }
  }
}

/* Call this first before performing colorize_partial.  This initializes the coloring table for l=1. */
void CYK::init_colorize_partial() {
  // populate color_table and score_table with initial sets for l = 1
  DEBUG_PRINT(("init\n"));
  for (int i = 0; i < m; ++i) {
    for (int nt = 1; nt < n; ++nt) {
      if (!is_ignored(nt) && get_cyk(nt, i, 1)) {
        set_color(i, 1, nt, i, 1);
        set_score(i, 1, 1, 1, -1);
      }
    }
  }
}

/* Analogous to parse_partial, but for coloring. */
int CYK::colorize_partial(int l) {
  // now compute colors for spans with l > 1
  DEBUG_PRINT(("colorize\n"));
  int next_l = l + 10;
  for (; l < lmax && l < next_l; ++l) {
    for (int i = 0; i <= m-l; ++i) {
      compute_color(i, l);
    }
  }

  if (l == lmax) {
    return 0;
  } else {
    return next_l;
  }
}

void CYK::print_cyk() {
  printf("Printing CYK table [n = %d, m = %d]\n", n, m);
  for (int nt = 0; nt < n; ++nt) {
    printf("=== %d ===\n", nt);
    printf("      ");
    for (int l = 0; l < lmax; ++l) {
      printf("%d ", l);
    }
    printf("\n");
    for (int i = 0; i < m; ++i) {
      printf("i=%d | ", i);
      for (int l = 0; l < lmax; ++l) {
        printf("%d ", cyk_table[nt][i][l]);
      }
      printf("\n");
    }
  }
}

void CYK::print_col() {
  printf("Printing Color table [n = %d, m = %d]\n", n, m);
  for (int l = 0; l < lmax; ++l) {
    for (int i = 0; i < m; ++i) {
      ColorSet &cs = col_table[i][l];
      if (cs.size() > 0) {
        printf("%d %d | ", i, l);
        for (int j = 0; j < cs.size(); ++j) {
          printf("[%d %d %d] ", cs.nt(j), cs.i(j), cs.l(j));
        }
        printf("\n");
      }
    }
  }
}

void CYK::print_info() {
  printf("n = %d\nm = %d\nlmax = %d\n", n, m, lmax);
  printf("Ignored (%d)\n", ignored.size());
}
