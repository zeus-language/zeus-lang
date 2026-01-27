//
// Created by stefan on 29.09.25.
//

#ifndef ZEUS_PARENT_LANGUAGESERVER_H
#define ZEUS_PARENT_LANGUAGESERVER_H
#include <filesystem>
#include <map>
#include <string>
#include <vector>

#include "CompilerOptions.h"
#include "lsp/messages.h"
#include "parser/module.h"
#include "parser/Parser.h"

struct LspDocument {
    std::string uri;
    std::string text;
};


class LanguageServer {
    lsp::LspOptions m_options;
    std::map<std::string, LspDocument> m_openDocuments;
    modules::ModuleCache m_moduleCache;

    lsp::requests::TextDocument_Definition::Result findDefinition(
        const lsp::requests::TextDocument_Definition::Params &params);
  [[nodiscard]] lsp::requests::TextDocument_Completion::Result findCompletions(
        const lsp::requests::TextDocument_Completion::Params &params) ;

public:
    explicit LanguageServer(lsp::LspOptions options);

    void handleRequest();
};


#endif //ZEUS_PARENT_LANGUAGESERVER_H
