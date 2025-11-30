//
// Created by stefan on 29.08.25.
//

#pragma once

#include <memory>
#include <optional>
#include <utility>

#include "ASTNode.h"
#include "lexer/Lexer.h"

namespace ast {
    enum class TypeModifier {
        REFERENCE,
        POINTER,
        NONE
    };

    struct RawType {
        Token typeToken;
        std::vector<Token> namespaceElements;
        TypeModifier typeModifier = TypeModifier::NONE;
        std::optional<Token> genericParam = std::nullopt;


        RawType(Token type_token, const std::vector<Token> &namespaceElements, const TypeModifier typeModifier,
                std::optional<Token> genericParam)
            : typeToken(std::move(type_token)), namespaceElements(namespaceElements),
              typeModifier(typeModifier), genericParam(std::move(genericParam)) {
        }

        virtual ~RawType() = default;

        [[nodiscard]] std::string fullTypeName() const {
            std::string name;

            name += typeToken.lexical();
            if (genericParam.has_value()) {
                name += "<" + genericParam.value().lexical() + ">";
            }
            return name;
        }

        [[nodiscard]] virtual std::unique_ptr<RawType> clone() const {
            return std::make_unique<RawType>(typeToken, namespaceElements, typeModifier, genericParam);
        }
    };

    struct ArrayRawType final : RawType {
        std::unique_ptr<RawType> baseType;
        size_t size;

        ArrayRawType(const Token &type_token, const std::vector<Token> &namespaceElements, TypeModifier typeModifier,
                     std::unique_ptr<RawType> base_type, size_t size)
            : RawType(type_token, namespaceElements, typeModifier, std::nullopt),
              baseType(std::move(base_type)),
              size(size) {
        }

        ~ArrayRawType() override = default;

        [[nodiscard]] std::unique_ptr<RawType> clone() const override {
            return std::make_unique<ArrayRawType>(typeToken, namespaceElements, typeModifier, baseType->clone(), size);
        }
    };

    class VariableDeclaration final : public ASTNode {
    private:
        std::unique_ptr<RawType> m_type;

        bool m_constant;
        std::optional<std::unique_ptr<ASTNode> > m_initialValue;

    public:
        explicit VariableDeclaration(Token name, std::unique_ptr<RawType> type, bool constant,
                                     std::optional<std::unique_ptr<ASTNode> > initialValue);

        ~VariableDeclaration() override = default;

        [[nodiscard]] std::optional<ASTNode *> initialValue() const;

        [[nodiscard]] RawType *type() const;

        [[nodiscard]] bool constant() const override;


        VariableDeclaration(VariableDeclaration &&) = default;

        VariableDeclaration(const VariableDeclaration &) = delete;

        VariableDeclaration &operator=(VariableDeclaration &&) = delete;

        VariableDeclaration &operator=(const VariableDeclaration &) = delete;

        std::optional<ASTNode *> getNodeByToken(const Token &token) const override {
            if (m_initialValue.has_value()) {
                return m_initialValue.value()->getNodeByToken(token);
            }
            return std::nullopt;
        }

        std::unique_ptr<ASTNode> clone() override {
            auto cloneNode = std::make_unique<VariableDeclaration>(expressionToken(),
                                                                   m_type->clone(),
                                                                   m_constant,
                                                                   m_initialValue.has_value()
                                                                       ? std::make_optional<std::unique_ptr<ASTNode> >(
                                                                           m_initialValue.value()->clone())
                                                                       : std::nullopt);
            if (expressionType())
                cloneNode->setExpressionType(expressionType().value());
            return cloneNode;
        }

        void makeNonGeneric(const std::shared_ptr<types::VariableType> &genericParam) override {
            ASTNode::makeNonGeneric(genericParam);
            if (m_initialValue.has_value()) {
                m_initialValue.value()->makeNonGeneric(genericParam);
            }
        }
    };
} // ast


