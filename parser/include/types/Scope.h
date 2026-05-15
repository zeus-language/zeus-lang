#pragma once
#include <unordered_map>
#include <optional>
#include <memory>
#include <string>
#include <variant>

#include "TypeRegistry.h"
#include "VariableType.h"
#include "ast/NumberConstant.h"

namespace types {
    typedef std::variant<ast::NumberValue, std::string> Constant;


    struct Variable {
        std::string name;
        std::shared_ptr<VariableType> type;
        bool constant;
        std::optional<Constant> value;
    };

    class Scope {
        std::shared_ptr<Scope> m_parentScope = nullptr;
        TypeRegistry m_typeRegistry;
        std::unordered_map<std::string, std::optional<Variable> > currentVariables;

    public:
        explicit Scope(const TypeRegistry &typeRegistry,
                       const std::shared_ptr<Scope> &parentScope);

        explicit Scope();

        virtual ~Scope();

        [[nodiscard]] std::optional<std::shared_ptr<VariableType> > getTypeByName(
            const std::string &name, const bool rawGenericName = false) const;

        void registerType(const std::shared_ptr<VariableType> &type);

        void registerTypeAlias(const std::string &name, const std::shared_ptr<VariableType> &type);

        void registerTypeInScope(const std::shared_ptr<VariableType> &type);

        void addVariable(const std::string &name, const Variable &var);

        [[nodiscard]] std::optional<Variable> findVariable(const std::string &name, bool includeParent = true) const;

        std::shared_ptr<Scope> parentScope() { return m_parentScope; }

        std::vector<std::shared_ptr<VariableType> > registeredTypes() const;

        [[nodiscard]] bool isGlobalScope() const;

        std::optional<std::shared_ptr<types::VariableType> >
        getSliceType(const std::shared_ptr<VariableType> &baseType);

        std::optional<std::shared_ptr<VariableType> > getPointerType(
            const std::shared_ptr<VariableType> &base_type);

        std::optional<std::shared_ptr<VariableType> > getReferenceType(
            const std::shared_ptr<VariableType> &base_type);

        std::optional<std::shared_ptr<VariableType> > getArrayType(
            const std::shared_ptr<VariableType> &base_type, size_t size);
    };
}
