#include <nix/store/store-api.hh>
#if NIX_IS_AT_LEAST(2, 32, 0)
#include <nix/store/build-result.hh>
#endif
#include <string>

namespace hercules {

class HerculesBuildError : public nix::BuildError {
public:
    nix::StorePath drv;
#if NIX_IS_AT_LEAST(2, 32, 0)
    HerculesBuildError(const std::string msg, nix::StorePath drv)
        : BuildError(nix::BuildResult::Failure::MiscFailure, msg), drv(drv) {};
#else
    HerculesBuildError(const std::string msg, nix::StorePath drv) : BuildError(msg), drv(drv) {};
#endif
};

void copyErrorStrings(const nix::Error &err, const char **msg, const char **trace) noexcept;

}
