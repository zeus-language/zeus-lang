#pragma once
#include <vector>

#include "ASTNode.h"

namespace ast {
    class UseModule final : public ASTNode {
    private:
        std::vector<Token> m_modulePath;
        std::optional<Token> m_alias;

    public:
        explicit UseModule(std::vector<Token> modulePath, std::optional<Token> aliasName) : ASTNode(modulePath.front()),
            m_modulePath(std::move(modulePath)), m_alias(std::move(aliasName)) {
        }

        ~UseModule() override = default;

        UseModule(UseModule &&) = default;

        UseModule(const UseModule &) = delete;

        UseModule &operator=(UseModule &&) = delete;

        UseModule &operator=(const UseModule &) = delete;

        [[nodiscard]] const std::vector<Token> &modulePath() const { return m_modulePath; }
        [[nodiscard]] std::optional<Token> alias() const { return m_alias; }

        [[nodiscard]] std::optional<std::string> aliasName() const {
            return (m_alias.has_value()) ? std::make_optional(m_alias.value().lexical()) : std::nullopt;
        }
    };
}
