//
// Created by stefan on 16.11.25.
//
#include "types/VariableType.h"

#include "ast/RawAnnotation.h"
#include "ast/FieldAccess.h"
#include "ast/FunctionDefinition.h"
#include "ast/FunctionSignature.h"


std::shared_ptr<types::VariableType> types::PointerType::makeNonGenericType(
    const std::shared_ptr<VariableType> &genericParam) {
    if (this->baseType()->typeKind() != TypeKind::GENERIC) {
        return std::make_shared<types::PointerType>(VariableType::name(), this->baseType());
    }
    return std::make_shared<types::PointerType>("*" + genericParam->name(), genericParam);
}

std::optional<std::shared_ptr<ast::FunctionSignature> >
types::InterfaceType::findMethod(const std::string &method_name) const {
    for (const auto &method: m_methods) {
        if (method->functionName() == method_name) {
            return std::make_optional(method);
        }
    }
    return std::nullopt;
}

std::optional<std::pair<std::shared_ptr<ast::FunctionSignature>, size_t> > types::InterfaceType::findMethodWithIndex(
    const std::string &method_name) const {
    for (size_t i = 0; i < m_methods.size(); ++i) {
        const auto &method = m_methods[i];
        if (method->functionName() == method_name) {
            return std::make_optional(std::make_pair(method, i));
        }
    }
    return std::nullopt;
}

bool types::InterfaceType::compare(const VariableType &other) const {
    if (auto otherInterface = dynamic_cast<const InterfaceType *>(&other)) {
        if (this->name() != otherInterface->name()) {
            return false;
        }
        if (this->genericParam().has_value() != otherInterface->genericParam().has_value()) {
            return false;
        }
        if (this->genericParam().has_value() && otherInterface->genericParam().has_value()) {
            if (this->genericParam().value()->name() != otherInterface->genericParam().value()->name()) {
                return false;
            }
        }
        return true;
    } else if (auto otherStruct = dynamic_cast<const StructType *>(&other)) {
        for (const auto &interface: otherStruct->interfaces()) {
            if (*interface == *this) {
                return true;
            }
        }
    }
    return false;
}

types::StructType::StructType(std::string name, const std::vector<StructField> &fields,
                              const std::vector<std::weak_ptr<ast::FunctionDefinition> > &methods,
                              const std::vector<std::shared_ptr<InterfaceType> > &interfaces,
                              std::optional<std::shared_ptr<VariableType> > genericParam) : VariableType(
        std::move(name),
        TypeKind::STRUCT), m_fields(fields),
    m_methods(methods), m_genericParam(std::move(genericParam)), m_interfaces(interfaces) {
    m_typename = VariableType::name() + (
                     m_genericParam.has_value() ? "<" + m_genericParam.value()->name() + ">" : "");
    m_linkageName = VariableType::name() + (m_genericParam.has_value() ? "_" + m_genericParam.value()->name() : "");
}

const ast::FunctionDefinition *types::StructType::getMethodByName(const std::string &methodName) const {
    for (const auto &method: m_methods) {
        if (method.lock()->functionName() == methodName) {
            return method.lock().get();
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
    std::vector<std::shared_ptr<ast::FunctionDefinition> > methods;

    for (const auto &methodPtr: this->m_methods) {
        auto method = methodPtr.lock();
        auto returnType = std::make_optional(method->returnType().value()->clone());
        Token token = method->expressionToken();
        std::vector<ast::FunctionArgument> args;
        for (auto &argOld: method->args()) {
            auto arg = ast::FunctionArgument(argOld.name,
                                             (argOld.rawType)
                                                 ? std::make_optional(argOld.rawType.value()->clone())
                                                 : std::nullopt, argOld.isConstant);
            if (argOld.type && argOld.type.value()->typeKind() == TypeKind::GENERIC) {
                arg.type = genericParam;
            } else {
                arg.type = argOld.type;
            }


            args.push_back(std::move(arg));
        }
        auto statements = method->block()->cloneBlock();

        auto annotations = std::vector<std::shared_ptr<ast::RawAnnotation> >();
        for (auto &annotationOld: method->rawAnnotations()) {
            annotations.push_back(std::move(annotationOld->cloneAnnotation()));
        }

        auto functionClone = std::make_shared<ast::FunctionDefinition>(
            std::move(token),
            std::move(args),
            std::move(returnType),
            std::move(statements),
            method->getGenericParam(),
            std::move(annotations),
            method->visibilityModifier()
        );
        functionClone->setParentStruct(this);

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


        for (auto &stmt: functionClone->block()->statements()) {
            stmt->makeNonGeneric(genericParam);
        }
        methods.push_back(functionClone);
    }

    auto structType = std::make_shared<NonGenericStructType>(VariableType::rawTypeName(), fields, std::move(methods));

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

void types::StructType::setMethods(const std::vector<std::shared_ptr<ast::FunctionDefinition> > &methods) {
    m_methods.clear();
    for (const auto &method: methods) {
        m_methods.push_back(method);
    }
}

size_t types::StructType::getInterfaceIndex(const std::shared_ptr<InterfaceType> &interface) const {
    for (size_t i = 0; i < m_interfaces.size(); ++i) {
        if (*m_interfaces[i] == *interface) {
            return i;
        }
    }
    throw std::runtime_error("Interface not implemented by struct");
}

types::SliceType::SliceType(std::string name, const std::shared_ptr<VariableType> &baseType) : StructType(
    std::move(name),
    {
        {
            .type = std::make_shared<types::IntegerType>("u64", 8, false),
            .name = "length"
        },
        {
            .type = std::make_shared<types::PointerType>("*" + baseType->name(), baseType),
            .name = "data"
        }
    },
    std::vector<std::weak_ptr<ast::FunctionDefinition> >{}, std::vector<std::shared_ptr<InterfaceType> >{},
    std::nullopt) {
    setTypeKind(TypeKind::SLICE);
}
