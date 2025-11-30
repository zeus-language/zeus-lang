#pragma once
#include <memory>
#include <optional>
#include <vector>

#include "VariableType.h"


namespace types {
    class TypeRegistry final {
    private:
        std::vector<std::shared_ptr<VariableType> > m_types;

    public:
        TypeRegistry();

        ~TypeRegistry() = default;

        std::vector<std::shared_ptr<VariableType> > registeredTypes();

        static std::optional<std::shared_ptr<VariableType> > getRangeType(
            const std::optional<std::shared_ptr<types::VariableType> >::value_type &value);

        static std::optional<std::shared_ptr<VariableType> > getArrayType(
            const std::shared_ptr<VariableType> &base_type, size_t size);

        static std::optional<std::shared_ptr<VariableType> > getPointerType(
            const std::shared_ptr<VariableType> &base_type);

        static std::optional<std::shared_ptr<VariableType> > getReferenceType(
            const std::shared_ptr<VariableType> &base_type);

        TypeRegistry(const TypeRegistry &) = delete;

        TypeRegistry &operator=(const TypeRegistry &) = delete;

        TypeRegistry(TypeRegistry &&) = delete;

        TypeRegistry &operator=(TypeRegistry &&) = delete;

        [[nodiscard]] std::optional<std::shared_ptr<VariableType> > getTypeByName(
            const std::string &name, bool rawGenericName) const;

        void registerType(const std::shared_ptr<VariableType> &type);
    };
}
