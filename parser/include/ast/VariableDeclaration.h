//
// Created by stefan on 29.08.25.
//

#pragma once

#include <memory>
#include <optional>
#include <utility>

#include "ASTNode.h"
#include "lexer/Lexer.h"
#include "types/VariableType.h"

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


        RawType(Token type_token, const std::vector<Token> &namespaceElements, const TypeModifier typeModifier)
            : typeToken(std::move(type_token)), namespaceElements(namespaceElements),
              typeModifier(typeModifier) {
        }

        virtual ~RawType() = default;
    };

    struct ArrayRawType final : public RawType {
        std::unique_ptr<RawType> baseType;
        size_t size;

        ArrayRawType(const Token &type_token, const std::vector<Token> &namespaceElements, TypeModifier typeModifier,
                     std::unique_ptr<RawType> base_type, size_t size)
            : RawType(type_token, namespaceElements, typeModifier),
              baseType(std::move(base_type)),
              size(size) {
        }

        ~ArrayRawType() override = default;
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
    };
} // ast


