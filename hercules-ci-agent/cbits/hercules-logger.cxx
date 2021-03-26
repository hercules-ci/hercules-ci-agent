#include "hercules-logger.hh"

HerculesLogger::HerculesLogger()
{

}

void HerculesLogger::push(std::unique_ptr<LogEntry> entry) {
  auto state(state_.lock());
  state->queue.push(std::move(entry));
  wakeup.notify_one();
}

uint64_t HerculesLogger::getMs() {
  auto t = std::chrono::steady_clock::now();
  auto millis = std::chrono::duration_cast<std::chrono::milliseconds>(t - t_zero).count();
  return millis;
}

void HerculesLogger::log(nix::Verbosity lvl, const nix::FormatOrString & fs) {
  push(std::make_unique<LogEntry>(LogEntry {
    .entryType = 1,
    .level = lvl,
    .ms = getMs(),
    .text = fs.s
  }));
}

#ifdef NIX_2_4
// TODO structured
void HerculesLogger::logEI(const nix::ErrorInfo & ei) {
  std::stringstream oss;
  showErrorInfo(oss, ei, false);
  log(ei.level, oss.str());
}
#endif

void HerculesLogger::startActivity(nix::ActivityId act, nix::Verbosity lvl, nix::ActivityType type,
    const std::string & s, const Fields & fields, nix::ActivityId parent) {
  push(std::make_unique<LogEntry>(LogEntry {
    .entryType = 2,
    .level = lvl,
    .ms = getMs(),
    .text = s,
    .activityId = act,
    .type = type,
    .parent = parent,
    .fields = fields
  }));
}

void HerculesLogger::stopActivity(nix::ActivityId act) {
  push(std::make_unique<LogEntry>(LogEntry {
    .entryType = 3,
    .ms = getMs(),
    .activityId = act
  }));
}

void HerculesLogger::result(nix::ActivityId act, nix::ResultType type, const Fields & fields) {
  push(std::make_unique<LogEntry>(LogEntry {
    .entryType = 4,
    .ms = getMs(),
    .activityId = act,
    .type = type,
    .fields = fields
  }));
}

void HerculesLogger::close() {
  auto state(state_.lock());
  state->queue.push(nullptr);
  wakeup.notify_one();
}

std::unique_ptr<HerculesLogger::LogEntry> HerculesLogger::pop () {
  auto state(state_.lock());

  while (state->queue.empty())
    state.wait(wakeup);

  auto r = std::move(state->queue.front());
  state->queue.pop();
  return r;
}

void HerculesLogger::popMany (int max, std::queue<std::unique_ptr<LogEntry>> &out) {
  auto state(state_.lock());

  while (state->queue.empty())
    state.wait(wakeup);

  for (int i = 0; i < max && !state->queue.empty(); i++) {
    out.push(std::move(state->queue.front()));
    state->queue.pop();
  }
}

HerculesLogger *herculesLogger = nullptr;
