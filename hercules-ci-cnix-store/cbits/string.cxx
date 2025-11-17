#include "hercules-ci-cnix/string.hxx"
#include <cstring>

namespace hercules_ci_cnix {

char * stringdup(const std::string & s) {
    char * p = (char *)malloc(s.size() + 1);
    std::memcpy(p, s.data(), s.size());
    p[s.size()] = '\0';
    return p;
}

} // namespace hercules_ci_cnix
