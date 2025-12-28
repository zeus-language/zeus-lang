#pragma once
#include <cassert>
#include <memory>
#include <optional>
#include <string>
#include <vector>


namespace ast {
    class FunctionDefinitionBase;
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
        POINTER,
        ARRAY,
        ENUM,
        GENERIC,
        FUNCTION
    };

    class VariableType {
    private:
        std::string m_typename;
        TypeKind m_typeKind;

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
            return this->name() == other.name();
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

    class PointerType final : public VariableType {
    private:
        std::shared_ptr<VariableType> m_baseType;

    public:
        PointerType(std::string name, std::shared_ptr<VariableType> baseType) : VariableType(
                std::move(name), TypeKind::POINTER),
            m_baseType(std::move(baseType)) {
        }

        [[nodiscard]] std::shared_ptr<VariableType> baseType() const { return m_baseType; }

        std::shared_ptr<VariableType> makeNonGenericType(const std::shared_ptr<VariableType> &genericParam) override;
    };

    class ReferenceType final : public VariableType {
    private:
        std::shared_ptr<VariableType> m_baseType;

    public:
        ReferenceType(std::string name, std::shared_ptr<VariableType> baseType) : VariableType(
                std::move(name), TypeKind::POINTER),
            m_baseType(std::move(baseType)) {
        }

        [[nodiscard]] std::shared_ptr<VariableType> baseType() const { return m_baseType; }
    };

    class ArrayType final : public VariableType {
    private:
        size_t m_size;
        std::shared_ptr<VariableType> m_baseType;

    public:
        ArrayType(std::string name, const size_t size, std::shared_ptr<VariableType> baseType) : VariableType(
                std::move(name), TypeKind::ARRAY),
            m_size(size), m_baseType(std::move(baseType)) {
        }

        [[nodiscard]] size_t size() const { return m_size; }

        [[nodiscard]] std::shared_ptr<VariableType> baseType() const { return m_baseType; }
    };

    struct StructField {
        std::shared_ptr<VariableType> type;
        std::string name;
    };

    class StructType final : public VariableType {
    private:
        std::vector<StructField> m_fields;
        std::vector<std::unique_ptr<ast::FunctionDefinitionBase> > m_methods;
        std::optional<std::shared_ptr<VariableType> > m_genericParam = std::nullopt;
        std::string m_typename;
        std::string m_linkageName;

    public:
        StructType(std::string name, const std::vector<StructField> &fields,
                   std::vector<std::unique_ptr<ast::FunctionDefinitionBase> > methods,
                   std::optional<std::shared_ptr<VariableType> > genericParam);

        [[nodiscard]] const std::vector<StructField> &fields() const { return m_fields; }

        [[nodiscard]] const std::vector<std::unique_ptr<ast::FunctionDefinitionBase> > &methods() const {
            return m_methods;
        }

        [[nodiscard]] std::optional<StructField> field(const std::string &fieldName) const {
            for (const auto &field: m_fields) {
                if (field.name == fieldName)
                    return field;
            }
            return std::nullopt;
        }

        [[nodiscard]] size_t getFieldIndexByName(const std::string &fieldName) const {
            for (size_t i = 0; i < m_fields.size(); ++i) {
                if (m_fields[i].name == fieldName) {
                    return i;
                }
            }
            assert(false && "invalid index for struct definition");
            return 0;
        }

        [[nodiscard]] std::optional<std::shared_ptr<VariableType> > genericParam() const {
            return m_genericParam;
        }

        [[nodiscard]] const std::string &name() const override {
            return m_typename;
        }

        [[nodiscard]] const std::string &linkageName() const override { return m_linkageName; }

        std::shared_ptr<VariableType> makeNonGenericType(
            const std::shared_ptr<VariableType> &genericParam) override;
    };


    struct EnumVariant {
        std::string name;
        int64_t value;
        // Additional fields can be added here for variant data
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

        [[nodiscard]] std::shared_ptr<VariableType> returnType() const {
            return m_returnType;
        }
    };
}
