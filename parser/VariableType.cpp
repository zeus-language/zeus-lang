//
// Created by stefan on 16.11.25.
//
#include "types/VariableType.h"

#include "ast/RawAnnotation.h"
#include "ast/FieldAccess.h"
#include "ast/FunctionDefinition.h"
#include "ast/FunctionSignature.h"
#include "types/Scope.h"


std::shared_ptr<types::VariableType> types::PointerType::makeNonGenericType(std::shared_ptr<types::Scope> &currentScope,
                                                                            const std::vector<std::shared_ptr<
                                                                                VariableType> > &genericParams) {
    if (this->baseType()->typeKind() != TypeKind::GENERIC) {
        return std::make_shared<types::PointerType>(VariableType::name(), this->baseType());
    }
    assert(genericParams.size() == 1 && "pointers can only have one generic param");
    if (this->baseType()->typeKind() == TypeKind::GENERIC)
        return std::make_shared<types::PointerType>("*" + genericParams[0]->name(), genericParams[0]);
    assert(false && "a pointer type is either generic or not");
    return nullptr;
}

std::shared_ptr<types::VariableType> types::ReferenceType::makeNonGenericType(
    std::shared_ptr<types::Scope> &currentScope, const std::vector<std::shared_ptr<VariableType> > &genericParams) {
    if (this->baseType()->typeKind() != TypeKind::GENERIC) {
        return std::make_shared<types::PointerType>(VariableType::name(), this->baseType());
    }
    assert(genericParams.size() == 1 && "pointers can only have one generic param");
    if (this->baseType()->typeKind() == TypeKind::GENERIC)
        return std::make_shared<types::ReferenceType>("&" + genericParams[0]->name(), genericParams[0]);
    assert(false && "a pointer type is either generic or not");
    return nullptr;
}

bool types::PointerType::isGeneric() {
    return baseType()->isGeneric();
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

static std::string getGenericParamName(const std::vector<std::shared_ptr<types::VariableType> > &genericParams) {
    if (genericParams.empty()) {
        return "";
    }
    std::string result = "<";
    for (size_t i = 0; i < genericParams.size(); ++i) {
        if (i > 0) {
            result += ", ";
        }
        result += genericParams[i]->name();
    }
    result += '>';
    return result;
}

static std::string getGenericParamLinkageName(const std::vector<std::shared_ptr<types::VariableType> > &genericParams) {
    if (genericParams.empty()) {
        return "";
    }
    std::string result;
    for (const auto &genericParam: genericParams) {
        result += "_" + genericParam->name();
    }
    return result;
}

const ast::FunctionDefinition *types::VariableTypeWithMethods::getMethodByName(const std::string &methodName) const {
    for (const auto &method: m_methods) {
        if (method->functionName() == methodName) {
            return method.get();
        }
    }
    return nullptr;
}

types::UnionType::UnionType(const std::string &name, const std::vector<UnionVariant> &m_variants,
                            const std::vector<std::shared_ptr<ast::FunctionDefinition> > &methods,
                            const std::vector<std::shared_ptr<VariableType> > &m_generic_params)
    : VariableTypeWithMethods(name, TypeKind::UNION, methods), m_variants(m_variants),
      m_genericParams(m_generic_params) {
    m_typename = VariableType::name() + getGenericParamName(m_genericParams);
    m_linkageName = VariableType::name() + getGenericParamLinkageName(m_genericParams);
}

std::optional<types::UnionVariant> types::UnionType::getVariant(const std::string &variantName) const {
    for (const auto &variant: m_variants) {
        if (variant.name == variantName) {
            return variant;
        }
    }
    return std::nullopt;
}

size_t types::UnionType::getVariantIndex(const std::string &variantName) const {
    for (size_t i = 0; i < m_variants.size(); ++i) {
        if (m_variants[i].name == variantName) {
            return i;
        }
    }
    throw std::invalid_argument("Variant not found");
}

std::shared_ptr<types::VariableType> types::UnionType::makeNonGenericType(std::shared_ptr<types::Scope> &currentScope,
                                                                          const std::vector<std::shared_ptr<
                                                                              VariableType> > &genericParams) {
    std::vector<UnionVariant> variants;
    std::vector<std::shared_ptr<VariableType> > nonGenericParams;
    std::vector<std::shared_ptr<ast::FunctionDefinition> > methods;

    auto unionType = std::make_shared<types::UnionType>(rawTypeName(), variants, methods, genericParams);
    if (auto type = currentScope->getTypeByName(unionType->name(), false)) {
        return type.value();
    }
    currentScope->registerType(unionType);

    for (auto variant: this->variants()) {
        auto new_variant = UnionVariant{variant.type, variant.associatedTypes, variant.fields, variant.name};
        for (size_t i = 0; i < new_variant.associatedTypes.size(); ++i) {
            auto type = variant.associatedTypes[i];
            for (size_t genIndex = 0; genIndex < genericParams.size(); ++genIndex) {
                if (this->m_genericParams.size() > genIndex and type == this->m_genericParams[genIndex]) {
                    new_variant.associatedTypes[i] = genericParams[genIndex];
                }
            }
        }

        for (auto field: new_variant.fields) {
            for (size_t genIndex = 0; genIndex < genericParams.size(); ++genIndex) {
                if (this->m_genericParams.size() > genIndex and field.type == this->m_genericParams[genIndex]) {
                    field.type = genericParams[genIndex];
                }
            }
        }
        variants.push_back(new_variant);
    }
    for (const auto &method: this->methods()) {
        auto returnType = std::make_optional(method->returnType().value()->clone());

        Token token = method->expressionToken();
        std::vector<ast::FunctionArgument> args;
        for (auto &argOld: method->args()) {
            auto arg = ast::FunctionArgument(argOld.name,
                                             (argOld.rawType)
                                                 ? std::make_optional(argOld.rawType.value()->clone())
                                                 : std::nullopt, argOld.isConstant);
            for (size_t genIndex = 0; genIndex < genericParams.size(); ++genIndex) {
                if (argOld.type && argOld.type.value() == m_genericParams[genIndex]) {
                    arg.type = genericParams[genIndex];
                    break;
                } else {
                    arg.type = argOld.type;
                }
            }


            args.emplace_back(arg);
        }
        auto statements = method->block()->cloneBlock();

        auto annotations = std::vector<std::shared_ptr<ast::RawAnnotation> >();
        for (auto &annotationOld: method->rawAnnotations()) {
            annotations.push_back(std::move(annotationOld->cloneAnnotation()));
        }
        auto genParam = method->getGenericParam();
        auto functionClone = std::make_shared<ast::FunctionDefinition>(
            token,
            std::move(args),
            std::move(returnType),
            std::move(statements),
            std::nullopt,
            std::move(annotations),
            method->visibilityModifier()
        );
        functionClone->setParentStruct(this);

        if (method->returnType()) {
            for (size_t genIndex = 0; genIndex < genericParams.size(); ++genIndex) {
                if (method->returnType().value()->hasGenericParam()) {
                    auto expr = method->expressionType().value();
                    functionClone->setExpressionType(expr->makeNonGenericType(currentScope, genericParams));
                } else if (method->expressionType() && method->expressionType().value() == this->m_genericParams[
                               genIndex]) {
                    functionClone->setExpressionType(genericParams[genIndex]);
                } else if (method->expressionType()) {
                    functionClone->setExpressionType(method->expressionType().value());
                }
            }
        }


        for (auto &stmt: functionClone->block()->statements()) {
            for (size_t genIndex = 0; genIndex < genericParams.size(); ++genIndex) {
                stmt->makeNonGeneric(currentScope, this->m_genericParams[genIndex], genericParams[genIndex]);
            }
        }
        methods.push_back(functionClone);
    }
    unionType->setMethods(methods);

    for (auto &method: unionType->methods()) {
        for (auto &arg: method->args()) {
            if (arg.name.lexical() == "self") {
                if (auto ptrType = std::dynamic_pointer_cast<types::PointerType>(arg.type.value())) {
                    arg.type = std::make_shared<types::PointerType>("*" + unionType->name(), unionType);
                }
                if (auto refType = std::dynamic_pointer_cast<types::ReferenceType>(arg.type.value())) {
                    arg.type = std::make_shared<types::ReferenceType>("&" + unionType->name(), unionType);
                } else {
                    arg.type = unionType;
                }
            }
        }
    }
    unionType->setVariants(variants);


    currentScope->registerType(unionType);

    return unionType;
}


bool types::UnionType::isGeneric() {
    return std::ranges::any_of(m_genericParams, [](const auto &param) {
        return param->typeKind() == TypeKind::GENERIC;
    });
}

void types::UnionType::setVariants(const std::vector<UnionVariant> &variants) {
    m_variants = variants;
}

types::StructType::StructType(const std::string &name, const std::vector<StructField> &fields,
                              const std::vector<std::shared_ptr<ast::FunctionDefinition> > &methods,
                              const std::vector<std::shared_ptr<InterfaceType> > &interfaces,
                              std::optional<std::shared_ptr<VariableType> > genericParam) : VariableTypeWithMethods(
        name,
        TypeKind::STRUCT, methods), m_fields(fields),
    m_genericParam(std::move(genericParam)), m_interfaces(interfaces) {
    m_typename = VariableType::name() + (
                     m_genericParam.has_value() ? "<" + m_genericParam.value()->name() + ">" : "");
    m_linkageName = VariableType::name() + (m_genericParam.has_value() ? "_" + m_genericParam.value()->name() : "");
}

std::shared_ptr<types::VariableType> types::StructType::makeNonGenericType(std::shared_ptr<types::Scope> &currentScope,
                                                                           const std::vector<std::shared_ptr<
                                                                               VariableType> > &genericParams) {
    auto fields = this->m_fields;

    for (auto &field: fields) {
        for (size_t genIndex = 0; genIndex < genericParams.size(); ++genIndex) {
            if (field.type == this->m_genericParam) {
                // TODO
                field.type = genericParams[0];
            }
        }
    }
    std::vector<std::shared_ptr<ast::FunctionDefinition> > methods;

    auto structType = std::make_shared<StructType>(VariableType::rawTypeName(), fields, std::move(methods),
                                                   interfaces(), genericParams.at(0));

    currentScope->registerType(structType);

    for (const auto &method: this->methods()) {
        auto returnType = std::make_optional(method->returnType().value()->clone());

        Token token = method->expressionToken();
        std::vector<ast::FunctionArgument> args;
        for (auto &argOld: method->args()) {
            auto arg = ast::FunctionArgument(argOld.name,
                                             (argOld.rawType)
                                                 ? std::make_optional(argOld.rawType.value()->clone())
                                                 : std::nullopt, argOld.isConstant);
            for (size_t genIndex = 0; genIndex < genericParams.size(); ++genIndex) {
                if (argOld.type && argOld.type.value() == m_genericParam) {
                    arg.type = genericParams[genIndex];
                    break;
                } else {
                    arg.type = argOld.type;
                }
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
            for (size_t genIndex = 0; genIndex < genericParams.size(); ++genIndex) {
                if (method->returnType().value()->hasGenericParam()) {
                    auto expr = method->expressionType().value();
                    functionClone->setExpressionType(expr->makeNonGenericType(currentScope, genericParams));
                } else if (method->expressionType() && method->expressionType().value() == this->m_genericParam) {
                    functionClone->setExpressionType(genericParams[genIndex]);
                } else {
                    functionClone->setExpressionType(method->expressionType().value());
                }
            }
        }


        for (auto &stmt: functionClone->block()->statements()) {
            for (size_t genIndex = 0; genIndex < genericParams.size(); ++genIndex) {
                stmt->makeNonGeneric(currentScope, this->m_genericParam.value(), genericParams[genIndex]);
            }
        }
        methods.push_back(functionClone);
    }


    structType->setMethods(methods);
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
    currentScope->registerType(structType);
    return structType;
}

size_t types::StructType::getInterfaceIndex(const std::shared_ptr<InterfaceType> &interface) const {
    for (size_t i = 0; i < m_interfaces.size(); ++i) {
        if (*m_interfaces[i] == *interface) {
            return i;
        }
    }
    throw std::runtime_error("Interface not implemented by struct");
}

std::shared_ptr<types::VariableType> types::SliceType::baseType() const {
    for (const auto &field: fields()) {
        if (field.name == "data") {
            if (auto ptrType = std::dynamic_pointer_cast<types::PointerType>(field.type)) {
                return ptrType->baseType();
            }
        }
    }
    throw std::runtime_error("Slice type does not have a data field");
}

types::SliceType::SliceType(std::string name, const std::shared_ptr<VariableType> &baseType) : StructType(
    std::move(name),
    {
        {
            .type = std::make_shared<types::IntegerType>("i32", 4, false),
            .name = "length"
        },
        {
            .type = std::make_shared<types::PointerType>("*" + baseType->name(), baseType),
            .name = "data"
        }
    },
    std::vector<std::shared_ptr<ast::FunctionDefinition> >{}, std::vector<std::shared_ptr<InterfaceType> >{},
    std::nullopt) {
    setTypeKind(TypeKind::SLICE);
}

bool types::GenericType::isGeneric() {
    return true;
}

std::shared_ptr<types::VariableType> types::GenericType::makeNonGenericType(std::shared_ptr<types::Scope> &currentScope,
                                                                            const std::vector<std::shared_ptr<
                                                                                VariableType> > &genericParams) {
    assert(genericParams.size() == 1);
    return genericParams.at(0);
}
