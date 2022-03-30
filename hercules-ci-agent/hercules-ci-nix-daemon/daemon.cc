#include <sys/wait.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>

#include <string_view>

#include <nix/config.h>
#include <nix/shared.hh>
#include <nix/error.hh>
#include <nix/util.hh>
#include <nix/globals.hh>
#include <nix/store-api.hh>
#include <nix/daemon.hh>

using namespace nix;

static void sigChldHandler(int sigNo)
{
    // Ensure we don't modify errno of whatever we've interrupted
    auto saved_errno = errno;
    //  Reap all dead children.
    while (waitpid(-1, 0, WNOHANG) > 0) ;
    errno = saved_errno;
}

static void setSigChldAction()
{
    struct sigaction act, oact;
    act.sa_handler = sigChldHandler;
    sigfillset(&act.sa_mask);
    act.sa_flags = 0;
    if (sigaction(SIGCHLD, &act, &oact))
        throw SysError("setting SIGCHLD handler");
}

extern "C" int main(int argc, char **argv) {
    nix::initNix();
    
    for (int i = 1; i < argc && argv[i]; i++) {
        std::string arg(argv[i]);
        if (arg == "--option") {
            if (i + 2 < argc) {
                nix::globalConfig.set(argv[i+1], argv[i+2]);
                i += 2;
            }
            else {
                std::cerr << "Not enough arguments to --option" << std::endl;
                return 1;
            }
        }
    }

    // See withNixDaemonProxy for stdin (ab)use
    AutoCloseFD socketFD = dup(0);
    closeOnExec(socketFD.get());

    // Put /dev/null on stdin
    int devnull = open("/dev/null", O_RDONLY);
    dup2(devnull, 0);
    close(devnull);

    setSigChldAction();
    while (1) {
        struct sockaddr_un remoteAddr;
        socklen_t remoteAddrLen = sizeof(remoteAddr);
        try {
            AutoCloseFD remote = accept(socketFD.get(), (struct sockaddr *) &remoteAddr, &remoteAddrLen);
            checkInterrupt();
            if (!remote) {
                if (errno == EINTR) continue;
                else throw SysError("hercules-ci-nix-daemon: accepting connection");
            }
            closeOnExec(remote.get());
            ProcessOptions options;
            options.errorPrefix = "hercules-ci-nix-daemon: unexpected error: ";
            options.dieWithParent = false;
            options.runExitHandlers = true;
            options.allowVfork = false;
            startProcess([&]() {
                socketFD = -1;

                // Regular nix daemon creates a new session for connections and
                // does not kill connection child processes. Those are not
                // appropriate behaviors for a temporary proxy.

                FdSource from(remote.get());
                FdSink to(remote.get());
                // TODO: disable caching without interfering with user parameters
                ref<Store> store = openStore();
                daemon::TrustedFlag trusted = daemon::NotTrusted;
                daemon::RecursiveFlag recursive = daemon::NotRecursive;
                std::function<void(Store &)> authHook = [](Store &){};
                daemon::processConnection(
                    store
                    , from
                    , to
                    , trusted
                    , recursive
                    , authHook
                    );
            });
        } catch (Interrupted & e) {
            return 0;
        } catch (Error & error) {
            ErrorInfo ei = error.info();
            ei.msg = hintfmt("hercules-ci-nix-daemon: error processing connection: %1%", ei.msg.str());
            logError(ei);
        }
    }
    return 0;
}
