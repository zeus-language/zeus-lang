#pragma once
#include <cassert>
#include <string>

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
        ARRAY
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

        [[nodiscard]] std::string name() const { return m_typename; }

        [[nodiscard]] TypeKind typeKind() const { return m_typeKind; }
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

    public:
        StructType(std::string name, const std::vector<StructField> &fields) : VariableType(std::move(name),
                                                                                   TypeKind::STRUCT), m_fields(fields) {
        }

        [[nodiscard]] const std::vector<StructField> &fields() const { return m_fields; }

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
    };
}
