//
// Created by stefan on 29.08.25.
//

#ifndef ZEUS_LANG_ASTNODE_H
#define ZEUS_LANG_ASTNODE_H
#include "lexer/Lexer.h"

namespace ast {
    class ASTNode {
    private:
        Token m_token;

    protected:
        explicit ASTNode(Token token) : m_token(std::move(token)) {
        }

    public:
        virtual ~ASTNode() = default;


        ASTNode(ASTNode &&) = default;

        ASTNode(const ASTNode &) = delete;

        ASTNode &operator=(ASTNode &&) = delete;

        ASTNode &operator=(const ASTNode &) = delete;


        virtual void type_check() {
        }

        [[nodiscard]] Token expressionToken() const { return m_token; }
    };
} // ast

#endif //ZEUS_LANG_ASTNODE_H
