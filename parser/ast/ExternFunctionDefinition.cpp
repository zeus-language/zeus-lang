//
// Created by stefan on 27.01.26.
//

#include "ast/ExternFunctionDefinition.h"

namespace ast {
    std::unique_ptr<ast::FunctionDefinitionBase> ExternFunctionDefinition::cloneFunction() {
        auto returnTypeClone = returnType().has_value()
                                   ? std::make_optional<std::unique_ptr<RawType> >(returnType().value()->clone())
                                   : std::nullopt;
        std::vector<std::unique_ptr<RawAnnotation> > annotationsClones;
        for (auto &annotation: rawAnnotations()) {
            annotationsClones.push_back(annotation->cloneAnnotation());
        }
        auto cloneNode = std::make_unique<ExternFunctionDefinition>(expressionToken(),
                                                                    args(),
                                                                    std::move(returnTypeClone),
                                                                    std::move(annotationsClones),
                                                                    visibilityModifier());
        if (expressionType())
            cloneNode->setExpressionType(expressionType().value());
        return cloneNode;
    }
}
