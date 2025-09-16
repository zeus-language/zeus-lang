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
    struct RawType {
        Token typeToken;
        bool isPointer = false;

        RawType(Token type_token, bool is_pointer)
            : typeToken(std::move(type_token)),
              isPointer(is_pointer) {
        }

        virtual ~RawType() = default;
    };

    struct ArrayRawType final : public RawType {
        std::unique_ptr<RawType> baseType;
        size_t size;

        ArrayRawType(const Token &type_token, bool is_pointer, std::unique_ptr<RawType> base_type, size_t size)
            : RawType(type_token, is_pointer),
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

        [[nodiscard]] bool constant() const;


        VariableDeclaration(VariableDeclaration &&) = default;

        VariableDeclaration(const VariableDeclaration &) = delete;

        VariableDeclaration &operator=(VariableDeclaration &&) = delete;

        VariableDeclaration &operator=(const VariableDeclaration &) = delete;
    };
} // ast


