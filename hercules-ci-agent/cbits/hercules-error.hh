#include <nix/store/store-api.hh>
#include <string>

namespace hercules {

class HerculesBuildError : public nix::BuildError {
public:
    nix::StorePath drv;
    HerculesBuildError(const std::string msg, nix::StorePath drv) : BuildError(msg), drv(drv) {};
};

void copyErrorStrings(const nix::Error &err, const char **msg, const char **trace) noexcept;

}
