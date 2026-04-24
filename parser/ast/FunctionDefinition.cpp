//
// Created by stefan on 29.08.25.
//

#include "ast/FunctionDefinition.h"

#include "ast/ReferenceAccess.h"

namespace ast {
    FunctionDefinition::FunctionDefinition(const Token &functionName, std::vector<FunctionArgument> args,
                                           std::optional<std::unique_ptr<RawType> > returnType,
                                           std::unique_ptr<BlockNode> blockNode,
                                           std::optional<Token> genericParam,
                                           std::vector<std::unique_ptr<RawAnnotation> > annotations,
                                           const VisibilityModifier visibilityModifier)
        : FunctionDefinitionBase(functionName, std::move(args), std::move(returnType),
                                 std::move(annotations), visibilityModifier),
          m_blockNode(std::move(blockNode)), genericParam(std::move(genericParam)) {
    }

    std::optional<ASTNode *> FunctionDefinition::getVariableDefinition(const Token &token) const {
        if (auto accessNode = m_blockNode->getVariableDefinition(token.lexical()); accessNode.has_value()) {
            return accessNode;
        }

        for (const auto &arg: args()) {
            if (arg.name == token) {
                return std::make_optional<ASTNode *>(const_cast<FunctionDefinition *>(this));
            }
        }
        return std::nullopt;
    }

    std::unique_ptr<ast::FunctionDefinitionBase> FunctionDefinition::cloneFunction() {
        return cloneFunction2();
    }

    std::unique_ptr<ast::FunctionDefinition> FunctionDefinition::cloneFunction2() {
        auto returnTypeClone = returnType().has_value()
                                   ? std::make_optional<std::unique_ptr<RawType> >(returnType().value()->clone())
                                   : std::nullopt;
        auto block = m_blockNode->cloneBlock();
        std::vector<std::unique_ptr<RawAnnotation> > annotationsClones;
        for (const auto &annotation: rawAnnotations()) {
            annotationsClones.push_back(annotation->cloneAnnotation());
        }
        auto cloneNode = std::make_unique<FunctionDefinition>(expressionToken(),
                                                              args(),
                                                              std::move(returnTypeClone),
                                                              std::move(block),
                                                              genericParam,
                                                              std::move(annotationsClones),
                                                              visibilityModifier());
        if (expressionType())
            cloneNode->setExpressionType(expressionType().value());
        if (resolvedReturnType())
            cloneNode->setResolvedReturnType(resolvedReturnType().value());
        for (const auto &annotation: annotations()) {
            cloneNode->addAnnotation(annotation);
        }
        cloneNode->setModulePath(modulePath());
        return std::move(cloneNode);
    }

    bool FunctionDefinition::isMethod() const {
        return m_parentStruct.has_value();
    }

    std::optional<ASTNode *> FunctionDefinition::getNodeByToken(const Token &token) const {
        if (auto result = FunctionDefinitionBase::getNodeByToken(token)) {
            return result;
        }
        for (const auto &arg: args()) {
            if (arg.name == token) {
                return std::make_optional<ASTNode *>(const_cast<FunctionDefinition *>(this));
            }
        }
        if (auto result = block()->getNodeByToken(token)) {
            return result;
        }
        return std::nullopt;
    }
} // ast
