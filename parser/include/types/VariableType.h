#pragma once
#include <cassert>
#include <memory>
#include <optional>
#include <string>
#include <vector>
#include <stdexcept>

#include "ast/VisibilityModifier.h"


namespace ast {
    class FunctionSignature;
    class FunctionDefinition;
    class IntrinsicFunctionDefinition;
}

namespace types {
    enum class TypeKind {
        INT,
        FLOAT,
        DOUBLE,
        STRING,
        BOOL,
        VOID,
        STRUCT,
        SLICE,
        POINTER,
        ARRAY,
        ENUM,
        GENERIC,
        FUNCTION,
        INTERFACE
    };

    class VariableType {
    private:
        std::string m_typename;
        TypeKind m_typeKind;

    protected:
        void setTypeKind(const TypeKind typeKind) { m_typeKind = typeKind; }

        virtual bool compare(const VariableType &other) const {
            return this->name() == other.name();
        }

    public:
        explicit VariableType(std::string name, const TypeKind typeKind) : m_typename(std::move(name)),
                                                                           m_typeKind(typeKind) {
        }

        virtual ~VariableType() = default;

        [[nodiscard]] const std::string &rawTypeName() const {
            return m_typename;
        }

        [[nodiscard]] virtual const std::string &name() const { return m_typename; }

        [[nodiscard]] virtual const std::string &linkageName() const { return m_typename; }

        [[nodiscard]] TypeKind typeKind() const { return m_typeKind; }

        bool operator==(const VariableType &other) const {
            return compare(other);
        }

        virtual std::shared_ptr<VariableType> makeNonGenericType(
            const std::shared_ptr<VariableType> &genericParam) {
            assert(false && "makeNonGenericType not implemented for this type");
            return nullptr;
        }
    };

    class IntegerType final : public VariableType {
    private:
        size_t m_size;
        bool m_signed;

    public:
        IntegerType(std::string name, const size_t size,
                    const bool sign) : VariableType(std::move(name), TypeKind::INT),
                                       m_size(size), m_signed(sign) {
        }

        [[nodiscard]] size_t size() const { return m_size; }

        [[nodiscard]] bool isSigned() const { return m_signed; }
    };

    class TypeWithBaseType : public VariableType {
    protected:
        std::shared_ptr<VariableType> m_baseType;

    public:
        TypeWithBaseType(std::string name, std::shared_ptr<VariableType> baseType) : VariableType(
                std::move(name), TypeKind::POINTER),
            m_baseType(std::move(baseType)) {
        }

        [[nodiscard]] std::shared_ptr<VariableType> baseType() const { return m_baseType; }
    };

    class PointerType final : public TypeWithBaseType {
    public:
        PointerType(std::string name, std::shared_ptr<VariableType> baseType) : TypeWithBaseType(
            std::move(name), std::move(baseType)) {
        }

        ~PointerType() override = default;


        std::shared_ptr<VariableType>
        makeNonGenericType(const std::shared_ptr<VariableType> &genericParam) override;

    protected:
        [[nodiscard]] bool compare(const VariableType &other) const override {
            if (auto otherPtrType = dynamic_cast<const PointerType *>(&other)) {
                return *this->baseType() == *otherPtrType->baseType();
            }
            return false;
        }
    };

    class ReferenceType final : public TypeWithBaseType {
    public:
        ReferenceType(std::string name, std::shared_ptr<VariableType> baseType) : TypeWithBaseType(
            std::move(name), std::move(baseType)) {
        }

        ~ReferenceType() override = default;

    protected:
        [[nodiscard]] bool compare(const VariableType &other) const override {
            if (auto otherPtrType = dynamic_cast<const ReferenceType *>(&other)) {
                return *this->baseType() == *otherPtrType->baseType();
            }
            return false;
        }
    };


    class ArrayType final : public VariableType {
    private:
        size_t m_size;
        std::weak_ptr<VariableType> m_baseType;

    public:
        ArrayType(std::string name, const size_t size, std::weak_ptr<VariableType> baseType) : VariableType(
                std::move(name), TypeKind::ARRAY),
            m_size(size), m_baseType(std::move(baseType)) {
        }

        [[nodiscard]] size_t size() const { return m_size; }

        [[nodiscard]] std::shared_ptr<VariableType> baseType() const { return m_baseType.lock(); }
    };

    struct StructField {
        ast::VisibilityModifier visibilityModifier;
        std::shared_ptr<VariableType> type;
        std::string name;
    };

    class InterfaceType final : public VariableType {
    private:
        std::vector<std::shared_ptr<ast::FunctionSignature> > m_methods;
        std::optional<std::shared_ptr<VariableType> > m_genericParam = std::nullopt;
        std::string m_typename;
        std::string m_linkageName;

    public:
        InterfaceType(const std::string &name,
                      const std::vector<std::shared_ptr<ast::FunctionSignature> > &methods,
                      const std::optional<std::shared_ptr<VariableType> > &generic_param = std::nullopt)
            : VariableType(name, TypeKind::INTERFACE),
              m_methods(methods),
              m_genericParam(generic_param) {
            m_typename = VariableType::name() + (
                             m_genericParam.has_value() ? "<" + m_genericParam.value()->name() + ">" : "");
            m_linkageName = VariableType::name() + (m_genericParam.has_value()
                                                        ? "_" + m_genericParam.value()->name()
                                                        : "");
        }

        [[nodiscard]] const std::vector<std::shared_ptr<ast::FunctionSignature> > &methods() const {
            return m_methods;
        }

        [[nodiscard]] const std::optional<std::shared_ptr<VariableType> > &genericParam() const {
            return m_genericParam;
        }

        [[nodiscard]] const std::string &name() const override {
            return m_typename;
        }

        [[nodiscard]] const std::string &linkageName() const override { return m_linkageName; }


        [[nodiscard]] std::optional<std::pair<std::shared_ptr<ast::FunctionSignature>, size_t> >
        findMethodWithIndex(
            const std::string &method_name) const;

    protected:
        [[nodiscard]] bool compare(const VariableType &other) const override;
    };

    class StructType : public VariableType {
    private:
        std::vector<std::shared_ptr<InterfaceType> > m_interfaces;
        std::vector<StructField> m_fields;
        std::vector<std::weak_ptr<ast::FunctionDefinition> > m_methods;
        std::optional<std::shared_ptr<VariableType> > m_genericParam = std::nullopt;
        std::string m_typename;
        std::string m_linkageName;

    public:
        StructType(std::string name, const std::vector<StructField> &fields,
                   const std::vector<std::weak_ptr<ast::FunctionDefinition> > &methods,
                   const std::vector<std::shared_ptr<InterfaceType> > &interfaces,
                   std::optional<std::shared_ptr<VariableType> > genericParam);

        ~StructType() override {
            m_fields.clear();
            m_methods.clear();
            m_genericParam.reset();
        }

        [[nodiscard]] const std::vector<StructField> &fields() const { return m_fields; }

        [[nodiscard]] virtual std::vector<std::shared_ptr<ast::FunctionDefinition> > methods() const {
            std::vector<std::shared_ptr<ast::FunctionDefinition> > methods;
            for (const auto &method: m_methods) {
                if (method.expired()) {
                    throw std::runtime_error("Method pointer expired for struct " + name());
                }
                if (auto methodPtr = method.lock()) {
                    methods.push_back(methodPtr);
                }
            }
            return methods;
        }

        [[nodiscard]] const ast::FunctionDefinition *getMethodByName(const std::string &methodName) const;

        [[nodiscard]] std::optional<StructField> field(const std::string &fieldName) const {
            for (const auto &field: m_fields) {
                if (field.name == fieldName)
                    return field;
            }
            return std::nullopt;
        }

        [[nodiscard]] size_t getFieldIndexByName(const std::string &fieldName) const {
            const size_t offset = m_interfaces.size();
            for (size_t i = 0; i < m_fields.size(); ++i) {
                if (m_fields[i].name == fieldName) {
                    return i + offset;
                }
            }
            assert(false && "invalid index for struct definition");
            return 0;
        }

        [[nodiscard]] const std::optional<std::shared_ptr<VariableType> > &genericParam() const {
            return m_genericParam;
        }

        [[nodiscard]] const std::string &name() const override {
            return m_typename;
        }

        [[nodiscard]] const std::string &linkageName() const override { return m_linkageName; }

        std::shared_ptr<VariableType> makeNonGenericType(
            const std::shared_ptr<VariableType> &genericParam) override;

        void setMethods(const std::vector<std::shared_ptr<ast::FunctionDefinition> > &methods);

        [[nodiscard]] std::vector<std::shared_ptr<InterfaceType> > interfaces() const { return m_interfaces; }

        size_t getInterfaceIndex(const std::shared_ptr<InterfaceType> &interface) const;

    protected:
        [[nodiscard]] bool compare(const VariableType &other) const override {
            if (auto otherPtrType = dynamic_cast<const InterfaceType *>(&other)) {
                for (const auto &interface: m_interfaces) {
                    if (*interface == *otherPtrType) {
                        return true;
                    }
                }
            }
            return this->name() == other.name();
        }
    };

    class SliceType final : public StructType {
    public:
        explicit SliceType(std::string name, const std::shared_ptr<VariableType> &baseType);
    };


    struct EnumVariant {
        std::string name;
        int64_t value;
        // Additional fields can be added here for variant data
    };

    class NonGenericStructType : public StructType {
        std::vector<std::shared_ptr<ast::FunctionDefinition> > m_methods;

    public:
        NonGenericStructType(std::string name, const std::vector<StructField> &fields,
                             const std::vector<std::shared_ptr<ast::FunctionDefinition> > &methods) : StructType(
                std::move(name), fields, {}, {}, std::nullopt),
            m_methods(methods) {
        }

        [[nodiscard]] std::vector<std::shared_ptr<ast::FunctionDefinition> > methods() const override {
            return m_methods;
        }
    };


    class EnumType final : public VariableType {
        std::vector<EnumVariant> m_variants;

    public:
        explicit EnumType(std::string name, std::vector<EnumVariant> variants) : VariableType(std::move(name),
                TypeKind::ENUM),
            m_variants(std::move(variants)) {
        }

        [[nodiscard]] const std::vector<EnumVariant> &variants() const {
            return m_variants;
        }

        std::optional<EnumVariant> getVariantByName(const std::string &name) const {
            for (const auto &variant: m_variants) {
                if (variant.name == name) {
                    return variant;
                }
            }
            return std::nullopt;
        }
    };

    struct GenericType final : public VariableType {
        std::string m_genericParam;

    public:
        explicit GenericType(const std::string &genericParam) : VariableType(genericParam,
                                                                             TypeKind::GENERIC),
                                                                m_genericParam(genericParam) {
        }
    };

    class FunctionType final : public VariableType {
        std::vector<std::shared_ptr<VariableType> > m_argumentTypes;
        std::shared_ptr<VariableType> m_returnType;

    public:
        FunctionType(std::string name,
                     std::vector<std::shared_ptr<VariableType> > argumentTypes,
                     std::shared_ptr<VariableType> returnType) : VariableType(std::move(name),
                                                                              TypeKind::FUNCTION),
                                                                 m_argumentTypes(std::move(argumentTypes)),
                                                                 m_returnType(std::move(returnType)) {
        }

        [[nodiscard]] const std::vector<std::shared_ptr<VariableType> > &argumentTypes() const {
            return m_argumentTypes;
        }

        [[nodiscard]] const std::shared_ptr<VariableType> &returnType() const {
            return m_returnType;
        }
    };
}
