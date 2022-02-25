
# A Cabal setup hook for pkg-config version info

Cabal doesn't natively give access to version information from `pkg-config` (yet?). This setup hook gets the version and makes it available via Haskell CPP and C/C++ macros, as well as setting Cabal flags automatically.
