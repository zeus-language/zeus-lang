//
// Created by stefan on 11.03.26.
//

#include "include/Environment.h"

#include "types/TypeRegistry.h"

void env::Environment::registerVariable(const std::string &name, const std::shared_ptr<types::VariableType> &type,
                                        ValueType value) {
    m_variables.emplace_back(name, type, value);
}

std::optional<env::Variable> env::Environment::findVariable(const std::string &name) const {
    for (const auto &var: m_variables) {
        if (var.name == name) {
            return var;
        }
    }
    return std::nullopt;
}

const std::vector<env::Variable> & env::Environment::variables() const {
    return m_variables;
}

env::Environment env::buildEnvironment() {
    Environment env;
    types::OsType osType;
#if defined(_WIN32) || defined(_WIN64)
    osType = types::OsType::WINDOWS;
#elif defined(__linux__)
    osType = types::OsType::LINUX;
#elif defined(__APPLE__)
    osType = types::OsType::MACOS;
#else
    osType = types::OsType::OTHER;
#endif
    env.registerVariable("target_os",types::createOstypeEnum(),static_cast<int>( osType));
    return env;
}
