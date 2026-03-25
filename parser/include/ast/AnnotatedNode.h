#pragma once
#include <utility>

#include "ASTNode.h"
#include "types/Annotation.h"


class AnnotatedNode : public ast::ASTNode {
private :
    std::vector<std::shared_ptr<types::Annotation> > m_annotations;

public:
    explicit AnnotatedNode(Token token)
        : ASTNode(std::move(token)) {
    }

    explicit AnnotatedNode(Token token, const ast::NodeType nodeType)
        : ASTNode(std::move(token), nodeType) {
    }

    ~AnnotatedNode() override = default;

    void addAnnotation(const std::shared_ptr<types::Annotation> &annotation) {
        m_annotations.push_back(annotation);
    }

    [[nodiscard]] const std::vector<std::shared_ptr<types::Annotation> > &annotations() const {
        return m_annotations;
    }

    [[nodiscard]] bool hasAnnotation(const std::string &annotationName) const {
        for (const auto &annotation: m_annotations) {
            if (annotation->name() == annotationName) {
                return true;
            }
        }
        return false;
    }

    template<typename T>
    [[nodiscard]] bool hasAnnotation() const {
        for (const auto &annotation: m_annotations) {
            if (auto casted = std::dynamic_pointer_cast<T>(annotation)) {
                return true;
            }
        }
        return false;
    }

    template<typename T>
    std::vector<std::shared_ptr<T> > getAnnotationsOfType() const {
        std::vector<std::shared_ptr<T> > result;
        for (const auto &annotation: m_annotations) {
            if (auto casted = std::dynamic_pointer_cast<T>(annotation)) {
                result.push_back(casted);
            }
        }
        return result;
    }
};

