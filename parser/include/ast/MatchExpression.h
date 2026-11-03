#pragma once

#include <memory>
#include <optional>

#include "ASTNode.h"
#include "DestructureStruct.h"
#include "DestructureTuple.h"
#include "lexer/Lexer.h"

namespace ast {
    struct MatchCase {
        std::vector<std::shared_ptr<ASTNode> > matchKeys;
        std::shared_ptr<ASTNode> expression;
    };

    class MatchExpression final : public ASTNode {
    private:
        std::shared_ptr<ASTNode> m_accessNode;
        std::vector<MatchCase> m_matchCases;

    public:
        explicit MatchExpression(Token name, std::shared_ptr<ASTNode> accessNode,
                                 std::vector<MatchCase> matchCases) : ASTNode(std::move(name),
                                                                              ast::NodeType::MATCH_EXPRESSION),
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

        std::shared_ptr<ASTNode> clone() override {
            std::vector<MatchCase> matchCaseClones;
            for (auto &[matchKeys, expression]: m_matchCases) {
                std::vector<std::shared_ptr<ASTNode> > keyClones;
                for (const auto &key: matchKeys) {
                    auto clonedKey = key->clone();
                    keyClones.push_back(clonedKey);
                }
                matchCaseClones.push_back(MatchCase{std::move(keyClones), expression->clone()});
            }
            auto cloneNode = std::make_shared<MatchExpression>(expressionToken(),
                                                               m_accessNode->clone(),
                                                               std::move(matchCaseClones));
            if (expressionType())
                cloneNode->setExpressionType(expressionType().value());

            for (auto &[matchKeys,_]: cloneNode->matchCases()) {
                for (auto &key: matchKeys) {
                    if (auto destruct = std::dynamic_pointer_cast<DestructureTuple>(key)) {
                        destruct->setAccessNode(cloneNode->accessNode());
                    } else if (auto destruct2 = std::dynamic_pointer_cast<DestructureStruct>(key)) {
                        destruct2->setAccessNode(cloneNode->accessNode());
                    }
                }
            }

            return std::move(cloneNode);
        }

        [[nodiscard]] bool hasReturnStatement() const override {
            for (auto &[matchKeys, expression]: m_matchCases) {
                if (expression->hasReturnStatement())
                    return true;
            }
            return false;
        }

        void makeNonGeneric(std::shared_ptr<types::Scope> &scope,
                            const std::shared_ptr<types::VariableType> &sourceParam,
                            const std::shared_ptr<types::VariableType> &genericParam) override {
            for (auto &[matchKeys, expression]: m_matchCases) {
                for (const auto &key: matchKeys) {
                    key->makeNonGeneric(scope, sourceParam, genericParam);
                }
                expression->makeNonGeneric(scope, sourceParam, genericParam);
            }
            m_accessNode->makeNonGeneric(scope, sourceParam, genericParam);
        }
    };
} // ast
