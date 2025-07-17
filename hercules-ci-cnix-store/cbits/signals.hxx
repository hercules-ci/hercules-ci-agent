#ifndef HERCULES_SIGNALS_H
#define HERCULES_SIGNALS_H

#ifdef __cplusplus
extern "C" {
#endif

// Install our C signal handler for the given signal
// This handler calls nix::triggerInterrupt() synchronously
// Returns 0 on success, -1 on error
int hercules_install_signal_handler(int sig);

#ifdef __cplusplus
}
#endif

#endif // HERCULES_SIGNALS_H