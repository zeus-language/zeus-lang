#pragma once
#include <memory>
#include <variant>

#include "types/VariableType.h"

namespace env {
    typedef std::variant<int64_t,double,std::string> ValueType;

    struct Variable {
        std::string name;
        std::shared_ptr<types::VariableType> type;
        ValueType value;
    };
    class Environment {
        std::vector< Variable > m_variables;

    public:
        Environment() = default;
        ~Environment() = default;
        void registerVariable(const std::string &name, const std::shared_ptr<types::VariableType> &type,ValueType value);
        [[nodiscard]]std::optional<Variable > findVariable(const std::string &name) const;

        [[nodiscard]] const std::vector<Variable>& variables() const;
    };


    Environment buildEnvironment();
}
