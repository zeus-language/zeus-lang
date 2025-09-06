#include  "types/TypeRegistry.h"

types::TypeRegistry::TypeRegistry() {
    registerType(std::make_shared<types::IntegerType>("i32", 4, true));
    registerType(std::make_shared<types::VariableType>("string", types::TypeKind::STRING));
    registerType(std::make_shared<types::VariableType>("void", types::TypeKind::VOID));
    registerType(std::make_shared<types::VariableType>("bool", types::TypeKind::BOOL));
    registerType(std::make_shared<types::IntegerType>("u32", 4, false));
    registerType(std::make_shared<types::IntegerType>("i64", 8, true));
    registerType(std::make_shared<types::IntegerType>("u64", 8, false));
    registerType(std::make_shared<types::VariableType>("float", types::TypeKind::FLOAT));
}

std::optional<std::shared_ptr<types::VariableType> > types::TypeRegistry::getTypeByName(const std::string &name) const {
    for (const auto &type: m_types) {
        if (type->name() == name) {
            return type;
        }
    }
    return std::nullopt;
}
