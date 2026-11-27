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
#include "Environment.h"
#include "lsp/messages.h"
#include "parser/module.h"
#include "parser/Parser.h"

// /home/stefan/projects/zeus-lang/cmake-build-debug/_deps/lsp-src/lsp
namespace lsp {
    struct CompletionList;
}

struct LspDocument {
    std::string uri;
    std::string text;
};


class LanguageServer {
    lsp::LspOptions m_options;
    env::Environment m_env;
    std::map<std::string, LspDocument> m_openDocuments;
    modules::ModuleCache m_moduleCache;

    lsp::requests::TextDocumentDefinition::Result findDefinition(
        const lsp::requests::TextDocumentDefinition::Params &params);

    lsp::requests::TextDocumentCompletion::Result findCompletionsForMembers(
        lsp::requests::TextDocumentCompletion::Result &result,
        lsp::CompletionList completionList, std::vector<Token> tokens,
        std::optional<Token> &foundToken, parser::ParseResult parseResult) const;

    [[nodiscard]] lsp::requests::TextDocumentCompletion::Result findCompletions(
        const lsp::requests::TextDocumentCompletion::Params &params);

    [[nodiscard]] lsp::requests::TextDocumentSemanticTokensFull::Result semanticTokensFull(
        const lsp::requests::TextDocumentSemanticTokensFull::Params &&params);

    [[nodiscard]] lsp::TextDocumentInlayHintResult resolveInlayHints(
        const lsp::requests::TextDocumentInlayHint::Params &&params);

    lsp::requests::CompletionItemResolve::Result resolveCompletionItem(
        const lsp::requests::CompletionItemResolve::Params &move);

public:
    explicit LanguageServer(lsp::LspOptions options, const env::Environment &env);

    void handleRequest();
};


#endif //ZEUS_PARENT_LANGUAGESERVER_H
