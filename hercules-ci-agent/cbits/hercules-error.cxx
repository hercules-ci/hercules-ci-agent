#include "hercules-error.hh"

void
hercules::copyErrorStrings(const nix::Error &err, const char **msgStrPtr, const char **traceStrPtr) noexcept {
    std::string msg;
    {
        std::stringstream s;
        nix::showErrorInfo(s, err.info(), false);
        msg = s.str();
        *msgStrPtr = strdup(msg.c_str());
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

        *traceStrPtr = strdup(t.c_str());
    }
}
