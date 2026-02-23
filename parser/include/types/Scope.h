#pragma once
#include <map>
#include <optional>
#include <memory>
#include <string>

#include "TypeRegistry.h"
#include "VariableType.h"

namespace types {
    struct Variable {
        std::string name;
        std::shared_ptr<VariableType> type;
        bool constant;
    };

    class Scope {
        std::shared_ptr<Scope> m_parentScope = nullptr;
        TypeRegistry m_typeRegistry;
        std::map<std::string, std::optional<Variable> > currentVariables;

    public:
        explicit Scope(const std::shared_ptr<Scope> &parentScope) : m_parentScope(parentScope) {
        }

        explicit Scope() : m_parentScope(nullptr) {
        }

        virtual ~Scope() = default;

        [[nodiscard]] std::optional<std::shared_ptr<VariableType> > getTypeByName(
            const std::string &name, const bool rawGenericName = false) const;

        void registerType(const std::shared_ptr<VariableType> &type);

        void registerTypeInScope(const std::shared_ptr<VariableType> &type);

        void addVariable(const std::string &name, const Variable &var);

        [[nodiscard]] std::optional<Variable> findVariable(const std::string &name,bool includeParent = true) const;

        std::shared_ptr<Scope> parentScope() { return m_parentScope; }

        std::vector<std::shared_ptr<VariableType> > registeredTypes();

         [[nodiscard]] bool isGlobalScope() const;
    };
}
