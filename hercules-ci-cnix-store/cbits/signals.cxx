#include <csignal>
#include <unistd.h>

#if NIX_IS_AT_LEAST(2, 28, 0)
#include <nix/util/signals.hh>
#else
#include <nix/config.h>
#include <nix/util.hh>
#  if NIX_IS_AT_LEAST(2,19,0)
#include <nix/signals.hh>
#  endif
#endif

#include "signals.hxx"

extern "C" {

// Storage for previous signal handlers
static struct sigaction old_sigint_action;
static struct sigaction old_sigterm_action;
static struct sigaction old_sighup_action;

// Get pointer to the old action structure for a given signal
static struct sigaction* get_old_action(int sig) {
    switch (sig) {
        case SIGINT: return &old_sigint_action;
        case SIGTERM: return &old_sigterm_action;
        case SIGHUP: return &old_sighup_action;
        default: return nullptr;
    }
}

// C signal handler that runs immediately in signal context
// This calls nix::triggerInterrupt() synchronously then defers to the previous handler
static void hercules_nix_signal_handler(int sig) {
    // Set the Nix interrupt flag immediately
    // This ensures checkInterrupt() calls after EINTR will work correctly
#if NIX_IS_AT_LEAST(2,24,0)
    nix::unix::triggerInterrupt();
#else
    nix::triggerInterrupt();
#endif
    
    // Call the previous handler to preserve existing behavior
    struct sigaction* old_action = get_old_action(sig);
    if (old_action && old_action->sa_handler != SIG_DFL && old_action->sa_handler != SIG_IGN) {
        if (old_action->sa_flags & SA_SIGINFO) {
            // Previous handler expects siginfo_t
            // We can't call it properly without the siginfo, so just call the basic handler
            if (old_action->sa_handler) {
                old_action->sa_handler(sig);
            }
        } else {
            // Previous handler is a simple function pointer
            old_action->sa_handler(sig);
        }
    }
}

// Install our C signal handler for the given signal, chaining to the previous handler
// Returns 0 on success, -1 on error
int hercules_install_signal_handler(int sig) {
    struct sigaction new_action;
    new_action.sa_handler = hercules_nix_signal_handler;
    sigemptyset(&new_action.sa_mask);
    new_action.sa_flags = 0;  // Ensure SA_RESTART is not set
    
    struct sigaction* old_action = get_old_action(sig);
    if (!old_action) {
        return -1;  // Unknown signal
    }
    
    // Install new handler and save the old one
    return sigaction(sig, &new_action, old_action);
}

} // extern "C"