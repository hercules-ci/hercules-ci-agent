#pragma once
#include <string>

namespace hercules_ci_cnix {

/**
  c_str() on a std::string only works if the std::string doesn't go out of scope.
  `stringdup` is like `strdup(s.c_str())`, but without the undefined behavior.
 */
char * stringdup(const std::string & s);

} // namespace hercules_ci_cnix
