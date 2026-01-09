#pragma once
#include "ASTNode.h"
#include "types/Annotation.h"


class AnnotatedNode : public ast::ASTNode {
private :
    std::vector<std::shared_ptr<types::Annotation> > m_annotations;

public:
    explicit AnnotatedNode(const Token &token)
        : ASTNode(token) {
    }

    ~AnnotatedNode() override = default;

    void addAnnotation(const std::shared_ptr<types::Annotation> &annotation) {
        m_annotations.push_back(annotation);
    }

    [[nodiscard]] const std::vector<std::shared_ptr<types::Annotation> > &annotations() const {
        return m_annotations;
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

