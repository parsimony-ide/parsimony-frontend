#ifndef _parser_h
#define _parser_h

#include <vector>
#include <set>

/** We store the grammar in an N by M by 3 table, where N is the number of symbols,
    and M is the maximum number of productions for a given LHS.

    All rules must be binary. (Singleton rules are excluded since they are handled
    by direct injection into the corresponding CYK table).
*/
class Grammar {
public:

  int n;        // number of symbols
  int m;        // max number of productions per LHS
  int ***table; // the grammar table

  Grammar(int n, int m);
  ~Grammar();

  void add(int l, int r1, int r2);
  int** productions_with_lhs(int l);
  void print();

};

/** A ColorSet represents a single entry in the coloring table. */
class ColorSet {
public:

  std::vector<int> nts;
  std::vector<int> is;
  std::vector<int> ls;

  ColorSet();

  void add(int nt, int i, int l);
  int nt(int i);
  int i(int i);
  int l(int i);
  size_t size();
};

class Score {
public:

  int coverage; // number of characters covered
  int largest;  // largest single color
  int num;      // number of extents

  Score();
  Score(int coverage, int largest, int num);

  bool better_than(Score &other);
  bool equals(Score &other);
  friend std::ostream & operator<<(std::ostream &strm, const Score &s);
};

class CYK {
private:

  int n;                 // 1 + number of symbols
  int m;                 // length of token string
  int lmax;              // 1+m
  bool ***cyk_table;     // the CYK table
  ColorSet **col_table;  // the coloring table
  Score **score_table;   // the color score table
  std::set<int> ignored; // set of ignored symbols
  Grammar &grammar;      // A binary grammar in CNF form

  bool match(int nt, int i, int l);                                    // compute one element of the CYK table
  void set_color(int i, int l, int nt, int ci, int cl);                // coloring table setter, single color
  void set_colors(int i, int l, ColorSet &colors);                     // coloring table setter, all colors are copied from supplied ColorSet
  Score & get_score(int i, int l);                                     // score table getter
  void set_score(int i, int l, int coverage, int largest, int num);    // score table setter
  void compute_color(int i, int l);                                    // compute one element of the coloring table
  bool is_ignored(int nt);

public:

  CYK(int n, int m, Grammar &g);
  ~CYK();

  void set_cyk(int nt, int i, int l);  // CYK table setter
  void unset_cyk(int nt, int i, int l);// CYK table setter
  bool get_cyk(int nt, int i, int l);  // CYK table getter
  int get_lmax();                      // lmax getter
  void parse();                        // fill out the CYK table
  int parse_partial(int l);            // fill out the CYK table for only some values of l
  ColorSet & get_colors(int i, int l); // coloring table getter
  void ignore(int nt);                 // mark the given nt as ignored, and thus excluded from colorings
  void colorize();                     // fill out the coloring table
  void init_colorize_partial();        // initialize the coloring table in preparation for colorize_partial
  int colorize_partial(int l);         // fill out the coloring table for only some values of l
  void print_cyk();                    // print the CYK table
  void print_col();                    // print the coloring table
  void print_info();                   // print general statistics
};

#endif
