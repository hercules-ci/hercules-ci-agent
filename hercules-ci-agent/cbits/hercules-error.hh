#if NIX_IS_AT_LEAST(2, 28, 0)

#include <nix/store/store-api.hh>

#else
#include <nix/config.h>
#include <nix/store-api.hh>
#endif

#include <string>

namespace hercules {

class HerculesBuildError : public nix::BuildError {
public:
    nix::StorePath drv;
    HerculesBuildError(const std::string msg, nix::StorePath drv) : BuildError(msg), drv(drv) {};
};

void copyErrorStrings(const nix::Error &err, const char **msg, const char **trace) noexcept;

}
