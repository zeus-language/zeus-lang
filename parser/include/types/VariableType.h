#pragma once
#include <string>

namespace types {
    enum class TypeKind {
        INT,
        FLOAT,
        STRING,
        BOOL,
        VOID,
        STRUCT
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
}
