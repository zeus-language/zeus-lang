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

    std::string joinTokenList(const std::vector<Token> &tokens);

    struct RawType {
    private:
        std::string m_fullTypeName;

    public:
        Token typeToken;
        std::vector<Token> namespaceElements;
        TypeModifier typeModifier = TypeModifier::NONE;
        std::vector<Token> genericParams;

        RawType(Token type_token, const std::vector<Token> &namespaceElements, const TypeModifier typeModifier,
                std::vector<Token> genericParams)
            : typeToken(std::move(type_token)), namespaceElements(namespaceElements),
              typeModifier(typeModifier), genericParams(std::move(genericParams)) {
            m_fullTypeName = typeToken.lexical();
            if (!this->genericParams.empty()) {
                m_fullTypeName += "<" + joinTokenList(this->genericParams) + ">";
            }
        }

        RawType(Token type_token, const std::vector<Token> &namespaceElements, const TypeModifier typeModifier,
                std::vector<Token> genericParams, std::string fullTypeName)
            : m_fullTypeName(std::move(fullTypeName)), typeToken(std::move(type_token)),
              namespaceElements(namespaceElements),
              typeModifier(typeModifier), genericParams(std::move(genericParams)) {
        }

        RawType(Token type_token, const TypeModifier typeModifier)
            : typeToken(std::move(type_token)), typeModifier(typeModifier) {
            m_fullTypeName = typeToken.lexical();
        }

        virtual ~RawType() = default;

        [[nodiscard]] const std::string &fullTypeName() const {
            return m_fullTypeName;
        }

        [[nodiscard]] virtual std::shared_ptr<RawType> clone() const {
            return std::make_unique<RawType>(typeToken, namespaceElements, typeModifier, genericParams, m_fullTypeName);
        }

        [[nodiscard]] bool hasGenericParam() const {
            return !genericParams.empty();
        }
    };

    struct PointerRawType final : RawType {
        std::shared_ptr<RawType> baseType;

        PointerRawType(const Token &type_token, const std::vector<Token> &namespaceElements, TypeModifier typeModifier,
                       std::shared_ptr<RawType> base_type)
            : RawType(type_token, namespaceElements, typeModifier, {}),
              baseType(std::move(base_type)) {
        }

        ~PointerRawType() override = default;

        [[nodiscard]] std::shared_ptr<RawType> clone() const override {
            return std::make_shared<PointerRawType>(typeToken, namespaceElements, typeModifier, baseType->clone());
        }
    };

    struct SliceRawType final : RawType {
        std::shared_ptr<RawType> baseType;

        SliceRawType(const Token &type_token, const std::vector<Token> &namespaceElements, TypeModifier typeModifier,
                     std::shared_ptr<RawType> base_type)
            : RawType(type_token, namespaceElements, typeModifier, {}),
              baseType(std::move(base_type)) {
        }

        ~SliceRawType() override = default;

        [[nodiscard]] std::shared_ptr<RawType> clone() const override {
            return std::make_shared<SliceRawType>(typeToken, namespaceElements, typeModifier, baseType->clone());
        }
    };

    struct ArrayRawType final : RawType {
        std::shared_ptr<RawType> baseType;
        size_t size;

        ArrayRawType(const Token &type_token, const std::vector<Token> &namespaceElements, TypeModifier typeModifier,
                     std::shared_ptr<RawType> base_type, size_t size)
            : RawType(type_token, namespaceElements, typeModifier, {}),
              baseType(std::move(base_type)),
              size(size) {
        }

        ~ArrayRawType() override = default;

        [[nodiscard]] std::shared_ptr<RawType> clone() const override {
            return std::make_shared<ArrayRawType>(typeToken, namespaceElements, typeModifier, baseType->clone(), size);
        }
    };

    struct FunctionRawType final : RawType {
        std::vector<std::shared_ptr<RawType> > argumentTypes;
        std::shared_ptr<RawType> returnType;

        FunctionRawType(const Token &type_token, std::vector<std::shared_ptr<RawType> > argument_types,
                        std::shared_ptr<RawType> return_type)
            : RawType(type_token, {}, TypeModifier::NONE, {}),
              argumentTypes(std::move(argument_types)),
              returnType(std::move(return_type)) {
        }

        ~FunctionRawType() override = default;

        [[nodiscard]] std::shared_ptr<RawType> clone() const override {
            std::vector<std::shared_ptr<RawType> > argTypesClones;
            for (const auto &argType: argumentTypes) {
                argTypesClones.push_back(argType->clone());
            }
            return std::make_shared<FunctionRawType>(typeToken, std::move(argTypesClones), returnType->clone());
        }
    };

    class VariableDeclaration final : public ASTNode {
    private:
        std::optional<std::shared_ptr<RawType> > m_type;

        bool m_constant;
        std::optional<std::shared_ptr<ASTNode> > m_initialValue;

    public:
        explicit VariableDeclaration(Token name, std::optional<std::shared_ptr<RawType> > type, bool constant,
                                     std::optional<std::shared_ptr<ASTNode> > initialValue);

        ~VariableDeclaration() override = default;

        [[nodiscard]] std::optional<ASTNode *> initialValue() const;

        [[nodiscard]] std::optional<RawType *> type() const;

        [[nodiscard]] bool constant() const override;


        VariableDeclaration(VariableDeclaration &&) = default;

        VariableDeclaration(const VariableDeclaration &) = delete;

        VariableDeclaration &operator=(VariableDeclaration &&) = delete;

        VariableDeclaration &operator=(const VariableDeclaration &) = delete;

        [[nodiscard]] std::optional<ASTNode *> getNodeByToken(const Token &token) const override {
            if (m_initialValue.has_value()) {
                return m_initialValue.value()->getNodeByToken(token);
            }
            return std::nullopt;
        }

        std::shared_ptr<ASTNode> clone() override;

        [[nodiscard]] VariableDeclaration copy() const;

        void makeNonGeneric(const std::shared_ptr<types::VariableType> &sourceParam,
                            const std::shared_ptr<types::VariableType> &genericParam) override {
            ASTNode::makeNonGeneric(sourceParam, genericParam);
            if (m_initialValue.has_value()) {
                m_initialValue.value()->makeNonGeneric(sourceParam, genericParam);
            }
        }
    };
} // ast
