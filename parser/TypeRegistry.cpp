#include  "types/TypeRegistry.h"

#include <algorithm>

#include "ast/FunctionDefinition.h"

std::shared_ptr<types::EnumType> types::createOstypeEnum() {
    std::vector<types::EnumVariant> variants = {
        {"WINDOWS", static_cast<int>(types::OsType::WINDOWS)},
        {"LINUX", static_cast<int>(types::OsType::LINUX)},
        {"MACOS", static_cast<int>(types::OsType::MACOS)},
        {"OTHER", static_cast<int>(types::OsType::OTHER)}
    };
    static auto ostype = std::make_shared<types::EnumType>("ostype", variants);
    return ostype;
}

types::TypeRegistry::TypeRegistry() {
    registerType(std::make_shared<types::IntegerType>("i32", 4, true));
    registerType(std::make_shared<types::VariableType>("void", types::TypeKind::VOID));
    registerType(std::make_shared<types::VariableType>("bool", types::TypeKind::BOOL));
    registerType(std::make_shared<types::IntegerType>("u8", 1, false));
    registerType(std::make_shared<types::IntegerType>("u16", 2, false));
    registerType(std::make_shared<types::IntegerType>("u32", 4, false));
    registerType(std::make_shared<types::IntegerType>("i8", 1, true));
    registerType(std::make_shared<types::IntegerType>("i16", 2, true));
    registerType(std::make_shared<types::IntegerType>("i64", 8, true));
    registerType(std::make_shared<types::IntegerType>("u64", 8, false));
    registerType(std::make_shared<types::IntegerType>("usize", 8, false));
    registerType(std::make_shared<types::VariableType>("float", types::TypeKind::FLOAT));
    registerType(std::make_shared<types::VariableType>("double", types::TypeKind::DOUBLE));
    registerType(createOstypeEnum());
}

types::TypeRegistry::~TypeRegistry() {
    m_types.clear();
    m_typeAliases.clear();
}

const std::vector<std::shared_ptr<types::VariableType> > &types::TypeRegistry::registeredTypes() const {
    return m_types;
}

std::optional<std::shared_ptr<types::VariableType> > types::TypeRegistry::getSliceType(
    const std::shared_ptr<VariableType> &value) {
    auto typeName = "[" + value->name() + "]";
    if (auto foundType = getTypeByName(typeName, false)) {
        return foundType;
    }

    auto slice = std::make_shared<types::SliceType>(typeName, value);
    registerType(slice);
    return std::make_optional(slice);
}

std::optional<std::shared_ptr<types::VariableType> > types::TypeRegistry::getPointerType(
    const std::shared_ptr<VariableType> &base_type) {
    auto typeName = "*" + base_type->name();
    if (auto foundType = getTypeByName(typeName, false)) {
        return foundType;
    }

    auto pointerType = std::make_shared<types::PointerType>(typeName, base_type);
    registerType(pointerType);
    return std::make_optional(pointerType);
}

std::optional<std::shared_ptr<types::VariableType> > types::TypeRegistry::getReferenceType(
    const std::shared_ptr<VariableType> &base_type) {
    auto typeName = "&" + base_type->name();
    if (auto foundType = getTypeByName(typeName, false)) {
        return foundType;
    }

    auto pointerType = std::make_shared<types::ReferenceType>(typeName, base_type);
    registerType(pointerType);
    return std::make_optional(pointerType);
}


std::optional<std::shared_ptr<types::VariableType> > types::TypeRegistry::getArrayType(
    const std::shared_ptr<VariableType> &base_type, size_t size) {
    auto typeName = "array[" + base_type->name() + "; " + std::to_string(size) + "]";
    if (auto foundType = getTypeByName(typeName, false)) {
        return foundType;
    }

    auto arrayType = std::make_shared<types::ArrayType>(typeName, size, base_type);
    registerType(arrayType);
    return std::make_optional(arrayType);
}

std::optional<std::shared_ptr<types::VariableType> > types::TypeRegistry::getTypeByName(
    const std::string &name, bool rawGenericName) const {
    if (const auto foundType = m_typeAliases.find(name); foundType != m_typeAliases.end()) {
        return foundType->second;
    }
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

void types::TypeRegistry::registerTypeAlias(const std::string &name, const std::shared_ptr<VariableType> &type) {
    m_typeAliases[name] = type;
}
