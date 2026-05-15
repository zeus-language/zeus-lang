//
// Created by stefan on 15.11.25.
//

#include "types/Scope.h"

#include <algorithm>

types::Scope::Scope(const TypeRegistry &typeRegistry,
                    const std::shared_ptr<Scope> &parentScope) : m_parentScope(parentScope),
                                                                 m_typeRegistry(typeRegistry) {
}

types::Scope::Scope() : m_parentScope(nullptr) {
}

types::Scope::~Scope() {
    m_parentScope = nullptr;
}

std::optional<std::shared_ptr<types::VariableType> > types::Scope::getTypeByName(
    const std::string &name, const bool rawGenericName) const {
    auto type = m_typeRegistry.getTypeByName(name, rawGenericName);
    if (type.has_value()) {
        return type;
    }

    if (m_parentScope != nullptr) {
        return m_parentScope->getTypeByName(name, rawGenericName);
    }

    return std::nullopt;
}

void types::Scope::registerType(const std::shared_ptr<VariableType> &type) {
    m_typeRegistry.registerType(type);
    if (m_parentScope != nullptr) {
        m_parentScope->registerType(type);
    }
}

void types::Scope::registerTypeAlias(const std::string &name, const std::shared_ptr<VariableType> &type) {
    m_typeRegistry.registerTypeAlias(name, type);
    if (m_parentScope != nullptr) {
        m_parentScope->registerTypeAlias(name, type);
    }
}

void types::Scope::registerTypeInScope(const std::shared_ptr<VariableType> &type) {
    m_typeRegistry.registerType(type);
}

void types::Scope::addVariable(const std::string &name, const Variable &var) {
    currentVariables[name] = var;
}

std::optional<types::Variable> types::Scope::findVariable(const std::string &name, bool includeParent) const {
    if (const auto it = currentVariables.find(name); it != currentVariables.end()) {
        return it->second;
    }

    if (m_parentScope != nullptr && includeParent) {
        return m_parentScope->findVariable(name);
    }

    return std::nullopt;
}

std::vector<std::shared_ptr<types::VariableType> > types::Scope::registeredTypes() const {
    auto types = m_typeRegistry.registeredTypes();
    if (m_parentScope != nullptr) {
        auto parentTypes = m_parentScope->registeredTypes();
        for (auto type: types) {
            std::erase_if(parentTypes,
                          [type](const auto &t) { return t->name() == type->name(); });
        }
        types.insert(types.end(), parentTypes.begin(), parentTypes.end());
    }
    return types;
}

bool types::Scope::isGlobalScope() const {
    return m_parentScope == nullptr;
}

std::optional<std::shared_ptr<types::VariableType> > types::Scope::getSliceType(
    const std::shared_ptr<VariableType> &baseType) {
    if (const auto type = m_typeRegistry.getSliceType(baseType); type.has_value()) {
        registerType(type.value());
        return type;
    }
    return std::nullopt;
}

std::optional<std::shared_ptr<types::VariableType> > types::Scope::getPointerType(
    const std::shared_ptr<VariableType> &base_type) {
    const auto type = m_typeRegistry.getPointerType(base_type);
    if (type.has_value()) {
        registerType(type.value());
        return type.value();
    }
    return std::nullopt;
}

std::optional<std::shared_ptr<types::VariableType> > types::Scope::getReferenceType(
    const std::shared_ptr<VariableType> &base_type) {
    return m_typeRegistry.getReferenceType(base_type);
}

std::optional<std::shared_ptr<types::VariableType> > types::Scope::getArrayType(
    const std::shared_ptr<VariableType> &base_type, size_t size) {
    return m_typeRegistry.getArrayType(base_type, size);
}
