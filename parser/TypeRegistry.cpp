#include  "types/TypeRegistry.h"

types::TypeRegistry::TypeRegistry() {
    registerType(std::make_shared<types::IntegerType>("i32", 4, true));
    registerType(std::make_shared<types::VariableType>("string", types::TypeKind::STRING));
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

std::optional<std::shared_ptr<types::VariableType> > types::TypeRegistry::getPointerType(
    const std::shared_ptr<VariableType> &base_type) {
    return std::make_optional(
        std::make_shared<types::PointerType>("ptr<" + base_type->name() + ">", base_type));
}

std::optional<std::shared_ptr<types::VariableType> > types::TypeRegistry::getArrayType(
    const std::shared_ptr<VariableType> &base_type, size_t size) {
    return std::make_optional(
        std::make_shared<types::ArrayType>("array[" + base_type->name() + "; " + std::to_string(size) + "]",
                                           size, base_type));
}

std::optional<std::shared_ptr<types::VariableType> > types::TypeRegistry::getTypeByName(const std::string &name) const {
    for (const auto &type: m_types) {
        if (type->name() == name) {
            return type;
        }
    }
    return std::nullopt;
}
