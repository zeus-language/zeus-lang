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

struct LspDocument {
    std::string uri;
    std::string text;
};


class LanguageServer {
    lsp::LspOptions m_options;
    std::map<std::string, LspDocument> m_openDocuments;

public:
    explicit LanguageServer(lsp::LspOptions options);

    void handleRequest();
};


#endif //ZEUS_PARENT_LANGUAGESERVER_H
