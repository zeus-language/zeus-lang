//
// Created by stefan on 29.08.25.
//

#ifndef ZEUS_LANG_ASTNODE_H
#define ZEUS_LANG_ASTNODE_H
#include <optional>
#include <utility>

#include "lexer/Lexer.h"
#include "types/VariableType.h"

namespace types {
    class Scope;
}

namespace ast {
    enum class NodeType {
        RETURN,
        VARIABLE_DECLARATION,
        VARIABLE_ASSIGNMENT,
        VARIABLE_ACCESS,
        FUNCTION_DEFINITION,
        FUNCTION_CALL,
        METHOD_CALL,
        IF_STATEMENT,
        IF_MACRO,
        WHILE_LOOP,
        FOR_LOOP,
        BLOCK,
        COMPARISSON,
        STRING_CONSTANT,
        NUMBER_CONSTANT,
        RAW_STRING_CONSTANT,
        TYPE_CAST,
        STRUCT_DECLARATION,
        STRUCT_INITIALIZATION,
        REFERENCE_ACCESS,
        ARRAY_ACCESS,
        MATCH_EXPRESSION,
        ENUM_DECLARATION,
        ENUM_VARIANT,
        UNION_DECLARATION,
        USE_MODULE,
        MODULE,
        ANNOTATION,
        GENERIC_PARAM,
        OTHER,
        DEREFERENCE,
        BINARY_EXPRESSION,
        BINARY_ASSIGN_EXPRESSION,
        INTERFACE_DECLARATION,
        DESTRUCTURE_TUPLE,
        DESTRUCTURE_STRUCT,
    };

    std::shared_ptr<types::VariableType> getOrMakeNonGenericType(const std::shared_ptr<types::VariableType> &sourceType,
                                                                 const std::shared_ptr<types::VariableType> &
                                                                 genericParam, std::shared_ptr<types::Scope> &scope);

    class ASTNode {
    private:
        Token m_token;
        NodeType m_nodeType = NodeType::OTHER;
        std::optional<std::shared_ptr<types::VariableType> > m_expressionType = std::nullopt;

    protected:
        explicit ASTNode(Token token, NodeType nodeType) : m_token(std::move(token)), m_nodeType(nodeType) {
        }

        explicit ASTNode(Token token) : m_token(std::move(token)) {
        }

    public:
        virtual ~ASTNode() = default;

        virtual std::shared_ptr<ASTNode> clone() {
            assert(false && "clone not implemented for this ASTNode type");
            return nullptr;
        }

        virtual void makeNonGeneric(std::shared_ptr<types::Scope> &scope,
                                    const std::shared_ptr<types::VariableType> &sourceParam,
                                    const std::shared_ptr<types::VariableType> &genericParam);


        ASTNode(ASTNode &&) = default;

        ASTNode(const ASTNode &) = delete;

        ASTNode &operator=(ASTNode &&) = delete;

        ASTNode &operator=(const ASTNode &) = delete;

        [[nodiscard]] std::optional<std::shared_ptr<types::VariableType> > expressionType() const {
            return (m_expressionType.has_value())
                       ? std::make_optional(m_expressionType.value())
                       : std::nullopt;
        }


        void setExpressionType(const std::shared_ptr<types::VariableType> &type) {
            assert(type != nullptr);
            m_expressionType = type;
        }

        [[nodiscard]] virtual bool constant() const {
            return false;
        }


        [[nodiscard]] const Token &expressionToken() const { return m_token; }

        [[nodiscard]] virtual std::optional<ASTNode *> getNodeByToken(const Token &token) const {
            if (m_token == token) {
                return const_cast<ASTNode *>(this);
            }
            return std::nullopt;
        }

        [[nodiscard]] NodeType nodeType() const { return m_nodeType; }
        [[nodiscard]] virtual bool hasReturnStatement() const { return false; }
    };
} // ast

#endif //ZEUS_LANG_ASTNODE_H
