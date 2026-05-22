#pragma once
#include "FunctionDefinitionBase.h"


namespace ast {
    class FunctionDefinition final : public FunctionDefinitionBase {
        std::shared_ptr<BlockNode> m_blockNode;
        std::optional<Token> genericParam;
        types::VariableType *m_parentStruct = nullptr;

    public:
        explicit FunctionDefinition(const Token &functionName, std::vector<FunctionArgument> args,
                                    std::optional<std::shared_ptr<RawType> > returnType,
                                    std::shared_ptr<BlockNode> blockNode,
                                    std::optional<Token> genericParam,
                                    std::vector<std::shared_ptr<RawAnnotation> > annotations,
                                    const VisibilityModifier visibilityModifier
        );

        ~FunctionDefinition() override;


        [[nodiscard]] BlockNode *block() const { return m_blockNode.get(); }

        [[nodiscard]] std::optional<ASTNode *> getVariableDefinition(const Token &token) const;


        FunctionDefinition(FunctionDefinition &&) = default;

        FunctionDefinition(const FunctionDefinition &) = delete;

        FunctionDefinition &operator=(FunctionDefinition &&) = delete;

        FunctionDefinition &operator=(const FunctionDefinition &) = delete;

        [[nodiscard]] std::optional<Token> getGenericParam() const {
            return genericParam;
        }

        std::shared_ptr<ast::FunctionDefinitionBase> cloneFunction() override;

        std::shared_ptr<ast::FunctionDefinition> cloneFunction2();

        [[nodiscard]] bool isMethod() const override;

        [[nodiscard]] std::optional<types::VariableType *> parentStruct() const override {
            return (m_parentStruct) ? std::make_optional(m_parentStruct) : std::nullopt;
        }

        void setParentStruct(types::VariableType *structType) {
            m_parentStruct = structType;
        }

        std::optional<ASTNode *> getNodeByToken(const Token &token) const override;
    };
} // ast

