//
// Created by stefan on 26.05.26.
//
#include "ast/FunctionSignature.h"

namespace ast {
    std::shared_ptr<FunctionSignature> FunctionSignature::cloneSignature() {
        auto returnTypeClone = returnType().has_value()
                                   ? std::make_optional<std::shared_ptr<RawType> >(returnType().value()->clone())
                                   : std::nullopt;
        std::vector<std::shared_ptr<RawAnnotation> > annotationsClones;
        for (auto &annotation: rawAnnotations()) {
            annotationsClones.push_back(annotation->cloneAnnotation());
        }


        auto cloneNode = std::make_shared<FunctionSignature>(expressionToken(),
                                                             args(),
                                                             std::move(returnTypeClone),
                                                             std::move(annotationsClones));
        for (const auto &annotation: annotations()) {
            cloneNode->addAnnotation(annotation);
        }
        if (expressionType())
            cloneNode->setExpressionType(expressionType().value());
        return std::move(cloneNode);
    }

    std::shared_ptr<ast::FunctionDefinitionBase> FunctionSignature::cloneFunction() {
        return cloneSignature();
    }

    bool FunctionSignature::isMethod() const {
        return true;
    }
}
