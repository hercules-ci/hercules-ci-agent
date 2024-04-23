#include "hercules-ci-cnix/string.hxx"

namespace hercules_ci_cnix {

char * stringdup(const std::string & s) {
    char * p = (char *)malloc(s.size() + 1);
    std::copy(s.begin(), s.end(), p);
    p[s.size()] = '\0';
    return p;
}

} // namespace hercules_ci_cnix
