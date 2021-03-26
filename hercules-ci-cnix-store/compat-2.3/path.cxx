
#include <nix/config.h>
#include <nix/store-api.hh>
#include <nix/path-compat.hh>
#include <nix/path.hh>
#include <nix-compat.hh>

using namespace nix;

static void checkName(std::string_view path, std::string_view name)
{
    if (name.empty())
        throw Error("store path '%s' has an empty name", path);
    if (name.size() > 211)
        throw Error("store path '%s' has a name longer than 211 characters", path);
    for (auto c : name)
        if (!((c >= '0' && c <= '9')
                || (c >= 'a' && c <= 'z')
                || (c >= 'A' && c <= 'Z')
                || c == '+' || c == '-' || c == '.' || c == '_' || c == '?' || c == '='))
            throw Error("store path '%s' contains illegal character '%s'", path, c);
}

StorePath::StorePath(std::string_view _baseName)
    : baseName(_baseName)
{
    if (baseName.size() < HashLen + 1)
        throw Error("'%s' is too short to be a valid store path", baseName);
    for (auto c : hashPart())
        if (c == 'e' || c == 'o' || c == 'u' || c == 't'
            || !((c >= '0' && c <= '9') || (c >= 'a' && c <= 'z')))
            throw Error("store path '%s' contains illegal base-32 character '%s'", baseName, c);
    checkName(baseName, name());
}

StorePath::StorePath(const Hash & hash, std::string_view _name)
    : baseName((hash.to_string(Base32, false) + "-").append(std::string(_name)))
{
    checkName(baseName, name());
}

std::pair<std::string_view, StringSet> nix::parsePathWithOutputs(std::string_view s)
{
    size_t n = s.find("!");
    return n == s.npos
        ? std::make_pair(s, std::set<string>())
        : std::make_pair(((std::string_view) s).substr(0, n),
            tokenizeString<std::set<string>>((std::string(s)).substr(n + 1), ","));
}

StorePathWithOutputs nix::parsePathWithOutputs(const Store &store, const std::string & s) {
    auto [path, outputs] = nix::parsePathWithOutputs(s);
    return {compatParseStorePath(store, std::string(path)), std::move(outputs)};
}
