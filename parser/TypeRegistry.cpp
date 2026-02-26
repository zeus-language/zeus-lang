#include  "types/TypeRegistry.h"

#include <algorithm>

#include "ast/FunctionDefinition.h"
#include "ast/VariableDeclaration.h"
#include "lexer/Lexer.h"

types::TypeRegistry::TypeRegistry() {
    registerType(std::make_shared<types::IntegerType>("i32", 4, true));
    registerType(std::make_shared<types::VariableType>("void", types::TypeKind::VOID));
    registerType(std::make_shared<types::VariableType>("bool", types::TypeKind::BOOL));
    registerType(std::make_shared<types::IntegerType>("u8", 1, false));
    registerType(std::make_shared<types::IntegerType>("u16", 2, false));
    registerType(std::make_shared<types::IntegerType>("u32", 4, false));
    registerType(std::make_shared<types::IntegerType>("i64", 8, true));
    registerType(std::make_shared<types::IntegerType>("u64", 8, false));
    registerType(std::make_shared<types::VariableType>("float", types::TypeKind::FLOAT));
    registerType(std::make_shared<types::VariableType>("double", types::TypeKind::DOUBLE));
}

std::vector<std::shared_ptr<types::VariableType> > types::TypeRegistry::registeredTypes() {
    return m_types;
}

std::optional<std::shared_ptr<types::VariableType> > types::TypeRegistry::getSliceType(
    const std::shared_ptr<VariableType> &value) {
    std::vector<StructField> fields = {
        {
            .type = std::make_shared<types::IntegerType>("u64", 8, false),
            .name = "length"
        },
        {
            .type = std::make_shared<types::PointerType>("*" + value->name(), value),
            .name = "data"
        }
    };
    std::vector<std::unique_ptr<ast::FunctionDefinition> > methods = {

    };
    std::optional<std::shared_ptr<VariableType> > genericParam;
    auto slice = std::make_shared<types::StructType>("slice", fields, std::move(methods),
                                                     std::nullopt);
    return std::make_optional(slice);
}

std::optional<std::shared_ptr<types::VariableType> > types::TypeRegistry::getPointerType(
    const std::shared_ptr<VariableType> &base_type) {
    return std::make_optional(
        std::make_shared<types::PointerType>("*" + base_type->name(), base_type));
}

std::optional<std::shared_ptr<types::VariableType> > types::TypeRegistry::getReferenceType(
    const std::shared_ptr<VariableType> &base_type) {
    return std::make_optional(
        std::make_shared<types::ReferenceType>("&" + base_type->name(), base_type));
}


std::optional<std::shared_ptr<types::VariableType> > types::TypeRegistry::getArrayType(
    const std::shared_ptr<VariableType> &base_type, size_t size) {
    return std::make_optional(
        std::make_shared<types::ArrayType>("array[" + base_type->name() + "; " + std::to_string(size) + "]",
                                           size, base_type));
}

std::optional<std::shared_ptr<types::VariableType> > types::TypeRegistry::getTypeByName(
    const std::string &name, bool rawGenericName) const {
    for (const auto &type: m_types) {
        if (rawGenericName && type->rawTypeName() == name) {
            return type;
        }
        if (type->name() == name) {
            return type;
        }
    }
    return std::nullopt;
}

void types::TypeRegistry::registerType(const std::shared_ptr<VariableType> &type) {
    if (std::ranges::none_of(m_types, [&type](const auto &t) { return t->name() == type->name(); })) {
        m_types.push_back(type);
    }
}
