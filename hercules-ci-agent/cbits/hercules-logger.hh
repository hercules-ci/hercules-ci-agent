#pragma once

#if NIX_IS_AT_LEAST(2, 28, 0)
#include <nix/util/error.hh>
#include <nix/util/logging.hh>
#include <nix/util/sync.hh>
#include <nix/main/shared.hh>
#else
#include <nix/config.h>
#include <nix/error.hh>
#include <nix/shared.hh>
#include <nix/sync.hh>
#include <nix/logging.hh>
#endif


#include <queue>
#include <string>

class HerculesLogger final : public nix::Logger {

public:
  struct LogEntry;

private:

  struct State {
    std::queue<std::unique_ptr<LogEntry>> queue;
  };
  nix::Sync<State> state_;
  std::condition_variable wakeup;

  void push(std::unique_ptr<LogEntry> entry);

 public:
  inline HerculesLogger() {};

  struct LogEntry {
    int entryType;
    int level;
    std::string text;
    uint64_t activityId;
    uint64_t type;
    uint64_t parent;
    Fields fields;
  };

#if NIX_IS_AT_LEAST(2, 15, 0)
  void log(nix::Verbosity lvl, const std::string_view s) override;
#else
  void log(nix::Verbosity lvl, const nix::FormatOrString & fs) override;
#endif
  void startActivity(nix::ActivityId act, nix::Verbosity lvl, nix::ActivityType type,
      const std::string & s, const Fields & fields, nix::ActivityId parent) override;
  void stopActivity(nix::ActivityId act) override;
  void result(nix::ActivityId act, nix::ResultType type, const Fields & fields) override;
  void logEI(const nix::ErrorInfo &ei) override;

  std::unique_ptr<LogEntry> pop();
  void popMany(int max, std::queue<std::unique_ptr<LogEntry>> &out);

  void close();
};

extern HerculesLogger *herculesLogger;
