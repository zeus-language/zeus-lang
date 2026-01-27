//
// Created by stefan on 27.01.26.
//

#ifndef ZEUS_PARENT_DBG_ASSERT_H
#define ZEUS_PARENT_DBG_ASSERT_H

#include <cassert>
#include <iostream>
#if NDEBUG
#define DBG_ASSERT(expr,message) \
if(!(expr)){\
std::cerr<< "DBG_ASSERT at " << __FILE__ << ":" << __LINE__ << " - " << message << "\n"; \
std::exit(1);\
}

#else
#define DBG_ASSERT(expr,message) \
if(!(expr)){\
std::cerr<< "DBG_ASSERT at " << __FILE__ << ":" << __LINE__ << " - " << message << "\n"; \
assert(expr && message);\
}
#endif
#endif //ZEUS_PARENT_DBG_ASSERT_H
