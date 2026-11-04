//
// Created by stefan on 29.08.25.
//

#include "../include/ast/ASTNode.h"

#include "types/Scope.h"

namespace ast {
    std::shared_ptr<types::VariableType> getOrMakeNonGenericType(const std::shared_ptr<types::VariableType> &sourceType,
                                                                 const std::shared_ptr<types::VariableType> &
                                                                 genericParam,
                                                                 std::shared_ptr<types::Scope> &scope) {
        if (!sourceType->isGeneric())
            return sourceType;
        const auto typeName = sourceType->rawTypeName() + "<" + genericParam->name() + ">";
        if (const auto nonGenericType = scope->getTypeByName(typeName, false))
            return nonGenericType.value();

        if (auto type = sourceType->makeNonGenericType(scope, {genericParam})) {
            scope->registerType(type.value());
            return type.value();
        }
        return nullptr;
    }

    void ASTNode::makeNonGeneric(std::shared_ptr<types::Scope> &scope,
                                 const std::shared_ptr<types::VariableType> &sourceParam,
                                 const std::shared_ptr<types::VariableType> &genericParam) {
        if (!m_expressionType.has_value()) {
            return;
        }
        if (m_expressionType.value() == sourceParam) {
            m_expressionType = genericParam;
        } else if (m_expressionType.value()->typeKind() == types::TypeKind::POINTER ||
                   m_expressionType.value()->typeKind() == types::TypeKind::ARRAY) {
            if (const auto ptrType = std::dynamic_pointer_cast<types::PointerType>(m_expressionType.value())) {
                if (ptrType->baseType() == sourceParam) {
                    auto newBaseType = genericParam;
                    const auto newPtrType = std::make_shared<types::PointerType>(
                        "*" + newBaseType->name(), newBaseType);
                    m_expressionType = newPtrType;
                }
            } else if (const auto refType = std::dynamic_pointer_cast<types::ReferenceType>(m_expressionType.value())) {
                if (refType->baseType() == sourceParam) {
                    auto newBaseType = genericParam;
                    const auto newPtrType = std::make_shared<types::ReferenceType>(
                        "&" + newBaseType->name(), newBaseType);
                    m_expressionType = newPtrType;
                } else if (refType->baseType()->isGeneric()) {
                    auto newBaseType = getOrMakeNonGenericType(refType->baseType(), genericParam, scope);
                    const auto newPtrType = std::make_shared<types::ReferenceType>(
                        "&" + newBaseType->name(), newBaseType);
                    m_expressionType = newPtrType;
                }
            }
        } else if (m_expressionType && expressionType().value()->isGeneric()) {
            auto newType = getOrMakeNonGenericType(m_expressionType.value(), genericParam, scope);
            m_expressionType = newType;
        }
    }
} // ast
