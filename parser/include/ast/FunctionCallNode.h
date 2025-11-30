//
// Created by stefan on 29.08.25.
//

#ifndef ZEUS_LANG_FUNCTIONCALLNODE_H
#define ZEUS_LANG_FUNCTIONCALLNODE_H
#include <memory>
#include <vector>

#include "ASTNode.h"
#include "lexer/Lexer.h"

namespace ast {
    class FunctionCallNode : public ASTNode {
        std::vector<Token> m_namespacePrefix;
        std::vector<std::unique_ptr<ASTNode> > m_args;
        std::optional<Token> m_genericParam;
        std::optional<std::shared_ptr<types::VariableType> > m_genericType = std::nullopt;

    public:
        explicit FunctionCallNode(Token functionName, std::vector<Token> namespacePrefix,
                                  std::vector<std::unique_ptr<ASTNode> > args, std::optional<Token> genericParam);

        ~FunctionCallNode() override = default;

        std::string functionName() const { return expressionToken().lexical(); }

        [[nodiscard]] std::string modulePathName() const {
            std::string name;
            for (auto &ns: m_namespacePrefix) {
                name += ns.lexical() + "::";
            }
            return name;
        }

        std::vector<std::unique_ptr<ASTNode> > &args() { return m_args; }

        [[nodiscard]] std::string functionSignature() const;

        [[nodiscard]] std::optional<Token> genericParam() const {
            return m_genericParam;
        }

        void setGenericType(const std::shared_ptr<types::VariableType> &genericType) {
            m_genericType = std::make_optional<std::shared_ptr<types::VariableType> >(genericType);
        }

        [[nodiscard]] std::optional<std::shared_ptr<types::VariableType> > genericType() const {
            return m_genericType;
        }


        FunctionCallNode(FunctionCallNode &&) = default;

        FunctionCallNode(const FunctionCallNode &) = delete;

        FunctionCallNode &operator=(FunctionCallNode &&) = delete;

        FunctionCallNode &operator=(const FunctionCallNode &) = delete;

        std::optional<ASTNode *> getNodeByToken(const Token &token) const override {
            for (auto &arg: m_args) {
                if (auto result = arg->getNodeByToken(token); result.has_value()) {
                    return result;
                }
            }
            const auto ownToken = expressionToken();
            return ownToken == token ? std::make_optional(const_cast<FunctionCallNode *>(this)) : std::nullopt;
        }

        std::unique_ptr<ASTNode> clone() override;

        void makeNonGeneric(const std::shared_ptr<types::VariableType> &genericParam) override {
            ASTNode::makeNonGeneric(genericParam);
            if (genericType()) {
                m_genericType = std::make_optional<std::shared_ptr<types::VariableType> >(genericParam);
            }
            for (auto &arg: m_args) {
                arg->makeNonGeneric(genericParam);
            }
        }
    };
} // ast

#endif //ZEUS_LANG_FUNCTIONCALLNODE_H
