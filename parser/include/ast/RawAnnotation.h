#pragma once
#include "ASTNode.h"

namespace ast {
    struct AnnotationArgument {
        std::string name;
        std::unique_ptr<ASTNode> value;
    };

    class RawAnnotation : public ASTNode {
    private:
        std::string m_name;
        std::vector<AnnotationArgument> m_args;

    public:
        RawAnnotation(Token nameToken, std::string name, std::vector<AnnotationArgument> args)
            : ASTNode(std::move(nameToken)), m_name(std::move(name)), m_args(std::move(args)) {
        }

        [[nodiscard]] const std::string &name() const { return m_name; }

        [[nodiscard]] const std::vector<AnnotationArgument> &args() const { return m_args; }

        [[nodiscard]] std::optional<std::unique_ptr<ASTNode> > getArgumentByName(const std::string &argName) const {
            for (const auto &arg: m_args) {
                if (arg.name == argName) {
                    return std::make_optional<std::unique_ptr<ASTNode> >(arg.value->clone());
                }
            }
            return std::nullopt;
        }

        std::unique_ptr<RawAnnotation> cloneAnnotation() {
            std::vector<AnnotationArgument> clonedArgs;
            for (const auto &arg: m_args) {
                clonedArgs.push_back(AnnotationArgument{
                    .name = arg.name,
                    .value = arg.value->clone()
                });
            }
            return std::make_unique<RawAnnotation>(expressionToken(), m_name, std::move(clonedArgs));
        }
    };
}
