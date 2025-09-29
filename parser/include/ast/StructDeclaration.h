#pragma once
#include <vector>

#include "ASTNode.h"
#include "VariableDeclaration.h"

namespace ast {
    struct StructField {
        Token name;
        std::unique_ptr<RawType> type;
    };

    class StructDeclaration final : public ASTNode {
    private:
        std::vector<StructField> m_fields;

    public:
        StructDeclaration(Token name, std::vector<StructField> fields) : ASTNode(std::move(name)),
                                                                         m_fields(std::move(fields)) {
        }

        ~StructDeclaration() override = default;

        StructDeclaration(StructDeclaration &&) = default;

        StructDeclaration(const StructDeclaration &) = delete;

        StructDeclaration &operator=(StructDeclaration &&) = delete;

        StructDeclaration &operator=(const StructDeclaration &) = delete;

        [[nodiscard]] const std::vector<StructField> &fields() const { return m_fields; }
    };
}
