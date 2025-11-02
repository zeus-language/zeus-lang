#pragma once

#include <memory>
#include <optional>

#include "ASTNode.h"
#include "lexer/Lexer.h"

namespace ast {
    struct MatchCase {
        std::vector<std::unique_ptr<ASTNode> > matchKeys;
        std::unique_ptr<ASTNode> expression;
    };

    class MatchExpression final : public ASTNode {
    private:
        std::unique_ptr<ASTNode> m_accessNode;
        std::vector<MatchCase> m_matchCases;

    public:
        explicit MatchExpression(Token name, std::unique_ptr<ASTNode> accessNode,
                                 std::vector<MatchCase> matchCases) : ASTNode(std::move(name)),
                                                                      m_accessNode(std::move(accessNode)),
                                                                      m_matchCases(std::move(matchCases)) {
        }

        [[nodiscard]] ASTNode *accessNode() const {
            return m_accessNode.get();
        }

        ~MatchExpression() override = default;

        MatchExpression(MatchExpression &&) = default;

        MatchExpression(const MatchExpression &) = delete;

        MatchExpression &operator=(MatchExpression &&) = delete;

        MatchExpression &operator=(const MatchExpression &) = delete;

        [[nodiscard]] bool constant() const override {
            return m_accessNode->constant();
        }

        std::vector<MatchCase> &matchCases() {
            return m_matchCases;
        }

        [[nodiscard]] std::optional<ASTNode *> getNodeByToken(const Token &token) const override {
            auto result = m_accessNode->getNodeByToken(token);
            if (result.has_value()) {
                return result;
            }
            for (auto &[keys,expression]: m_matchCases) {
                for (auto &key: keys) {
                    result = key->getNodeByToken(token);
                    if (result) {
                        return result;
                    }
                }
                result = expression->getNodeByToken(token);
                if (result) {
                    return result;
                }
            }
            auto ownToken = expressionToken();
            return ownToken == token ? std::make_optional(const_cast<MatchExpression *>(this)) : std::nullopt;
        }
    };
} // ast


