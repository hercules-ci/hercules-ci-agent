#include "hercules-error.hh"
#include "hercules-ci-cnix/string.hxx"

using namespace hercules_ci_cnix;

void
hercules::copyErrorStrings(const nix::Error &err, const char **msgStrPtr, const char **traceStrPtr) noexcept {
    std::string msg;
    {
        std::stringstream s;
        nix::showErrorInfo(s, err.info(), false);
        msg = s.str();
        *msgStrPtr = stringdup(msg);
    }

    // There's no method for getting just the trace, short of
    // reinventing the printing ourselves. That didn't seem to
    // be worth the effort.
    {
        std::stringstream s;
        nix::showErrorInfo(s, err.info(), true);
        std::string t = s.str();

        // drop prefix msg
        if (t.rfind(msg, 0) == 0) {
            t.replace(0, msg.size(), "");
            t = nix::trim(t);
        }

        *traceStrPtr = stringdup(t);
    }
}
