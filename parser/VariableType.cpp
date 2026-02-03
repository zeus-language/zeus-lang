//
// Created by stefan on 16.11.25.
//
#include "types/VariableType.h"

#include "ast/RawAnnotation.h"
#include "ast/FieldAccess.h"
#include "ast/FunctionDefinition.h"


std::shared_ptr<types::VariableType> types::PointerType::makeNonGenericType(
    const std::shared_ptr<VariableType> &genericParam) {
    if (this->baseType()->typeKind() != TypeKind::GENERIC) {
        return std::make_shared<types::PointerType>(VariableType::name(), this->baseType());
    }
    return std::make_shared<types::PointerType>("*" + genericParam->name(), genericParam);
}

types::StructType::StructType(std::string name, const std::vector<StructField> &fields,
                              std::vector<std::unique_ptr<ast::FunctionDefinitionBase> > methods,
                              std::optional<std::shared_ptr<VariableType> > genericParam) : VariableType(
        std::move(name),
        TypeKind::STRUCT), m_fields(fields),
    m_methods(std::move(methods)), m_genericParam(std::move(genericParam)) {
    m_typename = VariableType::name() + (
                     m_genericParam.has_value() ? "<" + m_genericParam.value()->name() + ">" : "");
    m_linkageName = VariableType::name() + (m_genericParam.has_value() ? "_" + m_genericParam.value()->name() : "");
}

const ast::FunctionDefinitionBase * types::StructType::getMethodByName(const std::string &methodName) const {
    for (const auto &method: m_methods) {
        if (method->functionName() == methodName) {
            return method.get();
        }
    }
    return nullptr;
}

std::shared_ptr<types::VariableType> types::StructType::makeNonGenericType(
    const std::shared_ptr<VariableType> &genericParam) {
    auto fields = this->m_fields;
    for (auto &field: fields) {
        if (field.type->typeKind() == TypeKind::GENERIC) {
            field.type = genericParam;
        }
    }
    std::vector<std::unique_ptr<ast::FunctionDefinitionBase> > methods;

    for (const auto &method: this->m_methods) {
        if (auto funcDef = dynamic_cast<ast::FunctionDefinition *>(method.get())) {
            auto returnType = make_optional(funcDef->returnType().value()->clone());
            Token token = funcDef->expressionToken();
            std::vector<ast::FunctionArgument> args;
            for (auto &argOld: method->args()) {
                auto arg = ast::FunctionArgument(argOld.name, argOld.rawType->clone(), argOld.isConstant);
                if (argOld.type && argOld.type.value()->typeKind() == TypeKind::GENERIC) {
                    arg.type = genericParam;
                } else {
                    arg.type = argOld.type;
                }


                args.push_back(std::move(arg));
            }
            std::vector<std::unique_ptr<ast::ASTNode> > statements;
            // Clone statements
            for (auto &stmtOld: funcDef->statements()) {
                auto stmtClone = stmtOld->clone();
                statements.push_back(std::move(stmtClone));
            }
            auto annotations = std::vector<std::unique_ptr<ast::RawAnnotation> >();
            for (auto &annotationOld: funcDef->rawAnnotations()) {
                annotations.push_back(std::move(annotationOld->cloneAnnotation()));
            }

            auto functionClone = std::make_unique<ast::FunctionDefinition>(
                std::move(token),
                std::move(args),
                std::move(returnType),
                std::move(statements),
                funcDef->getGenericParam(),
                std::move(annotations)
            );

            if (method->returnType()) {
                if (method->returnType().value()->genericParam) {
                    auto expr = method->expressionType().value();
                    functionClone->setExpressionType(expr);
                } else if (method->expressionType() && method->expressionType().value()->typeKind() ==
                           TypeKind::GENERIC) {
                    functionClone->setExpressionType(genericParam);
                } else {
                    functionClone->setExpressionType(method->expressionType().value());
                }
            }


            for (auto &stmt: functionClone->statements()) {
                stmt->makeNonGeneric(genericParam);
            }
            methods.push_back(std::move(functionClone));
        }
    }

    auto structType = std::make_shared<StructType>(VariableType::rawTypeName(), fields, std::move(methods),
                                                   genericParam);

    for (auto &method: structType->methods()) {
        for (auto &arg: method->args()) {
            if (arg.name.lexical() == "self") {
                if (auto ptrType = std::dynamic_pointer_cast<types::PointerType>(arg.type.value())) {
                    arg.type = std::make_shared<types::PointerType>("*" + structType->name(), structType);
                }
                if (auto refType = std::dynamic_pointer_cast<types::ReferenceType>(arg.type.value())) {
                    arg.type = std::make_shared<types::ReferenceType>("&" + structType->name(), structType);
                } else {
                    arg.type = structType;
                }
            }
        }
    }
    return structType;
}
