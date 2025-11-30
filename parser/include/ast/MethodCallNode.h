#pragma once

#include <memory>
#include <vector>

#include "ASTNode.h"
#include "lexer/Lexer.h"

namespace ast {
    class MethodCallNode : public ASTNode {
        std::unique_ptr<ASTNode> m_instanceNode;
        std::vector<std::unique_ptr<ASTNode> > m_args;

    public:
        explicit MethodCallNode(Token functionName, std::unique_ptr<ASTNode> instanceNode,
                                std::vector<std::unique_ptr<ASTNode> > args) : ASTNode(std::move(functionName)),
                                                                               m_instanceNode(std::move(instanceNode)),
                                                                               m_args(std::move(args)) {
        }

        ~MethodCallNode() override = default;

        [[nodiscard]] std::string functionName() const { return expressionToken().lexical(); }


        std::vector<std::unique_ptr<ASTNode> > &args() { return m_args; }

        [[nodiscard]] std::string functionSignature() const {
            std::string signature;
            signature += functionName() + "(";
            for (size_t i = 0; i < m_args.size(); ++i) {
                if (m_args[i]->expressionType().has_value()) {
                    signature += m_args[i]->expressionType().value()->name();
                } else {
                    signature += "unknown";
                }
                if (i < m_args.size() - 1) {
                    signature += ", ";
                }
            }
            signature += ")";
            return signature;
        }


        MethodCallNode(MethodCallNode &&) = default;

        MethodCallNode(const MethodCallNode &) = delete;

        MethodCallNode &operator=(MethodCallNode &&) = delete;

        MethodCallNode &operator=(const MethodCallNode &) = delete;

        [[nodiscard]] ast::ASTNode *instanceNode() const { return m_instanceNode.get(); }

        [[nodiscard]] std::optional<ASTNode *> getNodeByToken(const Token &token) const override {
            for (auto &arg: m_args) {
                if (auto result = arg->getNodeByToken(token); result.has_value()) {
                    return result;
                }
            }
            const auto ownToken = expressionToken();
            return ownToken == token ? std::make_optional(const_cast<MethodCallNode *>(this)) : std::nullopt;
        }

        std::unique_ptr<ASTNode> clone() override {
            std::vector<std::unique_ptr<ASTNode> > argsClones;
            for (auto &arg: m_args) {
                argsClones.push_back(arg->clone());
            }
            auto cloneNode = std::make_unique<MethodCallNode>(expressionToken(),
                                                              m_instanceNode->clone(),
                                                              std::move(argsClones));
            if (expressionType())
                cloneNode->setExpressionType(expressionType().value());
            return cloneNode;
        }
    };
} // ast


