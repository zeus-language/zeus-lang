//
// Created by stefan on 29.08.25.
//

#ifndef ZEUS_LANG_ASTNODE_H
#define ZEUS_LANG_ASTNODE_H
#include <optional>

#include "lexer/Lexer.h"
#include "types/VariableType.h"

namespace ast {
    class ASTNode {
    private:
        Token m_token;
        std::optional<std::shared_ptr<types::VariableType> > m_expressionType = std::nullopt;

    protected:
        explicit ASTNode(Token token) : m_token(std::move(token)) {
        }

    public:
        virtual ~ASTNode() = default;


        ASTNode(ASTNode &&) = default;

        ASTNode(const ASTNode &) = delete;

        ASTNode &operator=(ASTNode &&) = delete;

        ASTNode &operator=(const ASTNode &) = delete;

        [[nodiscard]] std::optional<std::shared_ptr<types::VariableType> > expressionType() const {
            return m_expressionType;
        }

        void setExpressionType(std::shared_ptr<types::VariableType> type) {
            m_expressionType = std::make_optional<std::shared_ptr<types::VariableType> >(type);
        }


        [[nodiscard]] Token expressionToken() const { return m_token; }
    };
} // ast

#endif //ZEUS_LANG_ASTNODE_H
