#ifndef _debug_h
#define _debug_h

#include <iostream>

#ifdef DEBUG
#define DEBUG_PRINT(x) printf x
#define dout std::cout
#else
#define DEBUG_PRINT(x) do {} while (0)
#define dout 0 && std::cout
#endif

#endif
