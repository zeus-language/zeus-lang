#pragma once
#include <vector>

#include "ASTNode.h"

namespace ast {
    class UseModule final : public ASTNode {
    private:
        std::vector<Token> m_modulePath;

    public:
        explicit UseModule(std::vector<Token> modulePath) : ASTNode(modulePath.front()),
                                                            m_modulePath(std::move(modulePath)) {
        }

        ~UseModule() override = default;

        UseModule(UseModule &&) = default;

        UseModule(const UseModule &) = delete;

        UseModule &operator=(UseModule &&) = delete;

        UseModule &operator=(const UseModule &) = delete;

        [[nodiscard]] const std::vector<Token> &modulePath() const { return m_modulePath; }
    };
}
