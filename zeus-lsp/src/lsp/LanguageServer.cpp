//
// Created by stefan on 29.09.25.
//

#include "LanguageServer.h"

#include <lsp/connection.h>
#include <lsp/io/standardio.h>
#include <lsp/messagehandler.h>
#include <lsp/messages.h>

#include <future>
#include <iostream>
#include <string>
#include <thread>

#include "ast/FieldAccess.h"
#include "ast/FunctionCallNode.h"
#include "ast/FunctionDefinition.h"
#include "ast/MethodCallNode.h"
#include "ast/VariableAccess.h"
#include "lexer/Lexer.h"
#include "parser/module.h"
#include "parser/Parser.h"
#include "types/TypeChecker.h"

using namespace std::string_view_literals;

static lsp::DiagnosticSeverity mapOutputTypeToSeverity(const parser::OutputType output) {
    switch (output) {
        case parser::OutputType::ERROR:
            return lsp::DiagnosticSeverity::Error;
        case parser::OutputType::HINT:
            return lsp::DiagnosticSeverity::Hint;
        case parser::OutputType::WARN:
            return lsp::DiagnosticSeverity::Warning;
    }
    return lsp::DiagnosticSeverity::Information;
}

lsp::Uri toUri(const std::string &filePath) {
    if (filePath.starts_with("file:")) {
        return lsp::Uri::parse(filePath);
    } else {
        return lsp::FileUri::fromPath(filePath);
    }
}

static std::vector<lsp::Diagnostic> buildDiagnosticsFromMessages(
    const std::map<std::string, std::vector<parser::ParserMessasge> > &messages,
    const std::string &targetUri) {
    std::vector<lsp::Diagnostic> diagnostics;

    auto it = messages.find(targetUri);
    if (it == messages.end())
        return diagnostics;

    for (const auto &msg: it->second) {
        const auto &token = msg.token;
        lsp::Diagnostic diag;
        // adjust to 0-based indexing

        diag.range.start.line = static_cast<int>(token.source_location.row) - 1;
        diag.range.start.character = static_cast<int>(token.source_location.col) - 1;
        diag.range.end.line = static_cast<int>(token.source_location.row) - 1;
        diag.range.end.character = static_cast<int>(token.source_location.col + token.source_location.num_bytes) - 1;
        diag.severity = mapOutputTypeToSeverity(msg.outputType);
        diag.message = msg.message;
        diag.source = std::string("zeus");
        diag.relatedInformation = std::vector<lsp::DiagnosticRelatedInformation>{
            lsp::DiagnosticRelatedInformation{
                .location = lsp::Location{
                    .uri = toUri(token.source_location.filename),
                    .range = diag.range
                },
                .message = msg.message
            }
        };
        diagnostics.push_back(std::move(diag));
    }

    return diagnostics;
}


static std::map<std::string, std::vector<parser::ParserMessasge> > collectDiagnostics(
    const std::vector<std::filesystem::path> &rtlDirectories,
    const lsp::Uri &uri,
    const std::string &text) {
    std::map<std::string, std::vector<parser::ParserMessasge> > errorsMap;
    auto tokens = lexer::lex_file(std::string(uri.path()), text);
    auto result = parser::parse_tokens(tokens);
    modules::include_modules(rtlDirectories, result);

    types::TypeCheckResult type_check_result;
    types::type_check(result.module, type_check_result);

    if (result.messages.empty() && type_check_result.messages.empty()) {
        errorsMap[std::string(uri.path())] = {};
    } else {
        for (auto &error: result.messages) {
            errorsMap[error.token.source_location.filename].push_back(error);
        }
        for (const auto &msg: type_check_result.messages) {
            errorsMap[msg.token.source_location.filename].push_back(msg);
        }
    }

    return errorsMap;
}

LanguageServer::LanguageServer(lsp::LspOptions options) : m_options(std::move(options)) {
}

void LanguageServer::handleRequest() {
    try {
        auto connection = lsp::Connection(lsp::io::standardIO());
        auto messageHandler = lsp::MessageHandler(connection);

        // running flag
        std::atomic<bool> running{true};

        // Register callbacks
        messageHandler.add<lsp::requests::Initialize>([&](lsp::requests::Initialize::Params &&params) {
                    lsp::InitializeResult result;
                    result.capabilities.positionEncoding = lsp::PositionEncodingKind::UTF8;

                    result.capabilities.textDocumentSync = lsp::TextDocumentSyncOptions{
                        .openClose = true,
                        .change = lsp::TextDocumentSyncKind::Full,
                        .save = true
                    };
                    result.capabilities.hoverProvider = false;
                    result.capabilities.definitionProvider = true;
                    result.capabilities.declarationProvider = true;
                    result.capabilities.diagnosticProvider = lsp::DiagnosticOptions{
                        .interFileDependencies = true,
                        .workspaceDiagnostics = false
                    };
                    result.capabilities.completionProvider = lsp::CompletionOptions{
                        .triggerCharacters = std::vector<std::string>{"."},
                        .resolveProvider = false,

                    };

                    result.serverInfo = lsp::InitializeResultServerInfo{.name = "zeusls", .version = "0.1"};

                    return lsp::requests::Initialize::Result{
                        .capabilities = result.capabilities, .serverInfo = result.serverInfo
                    };
                })
                .add<lsp::requests::Shutdown>([&]() {
                    return lsp::requests::Shutdown::Result();
                })
                .add<lsp::notifications::Exit>([&]() {
                    running = false;
                })
                .add<lsp::notifications::TextDocument_DidOpen>(
                    [&](lsp::notifications::TextDocument_DidOpen::Params &&params) {
                        // store document
                        auto uri = params.textDocument.uri;
                        auto uriString = params.textDocument.uri.toString();
                        auto text = params.textDocument.text;
                        m_openDocuments[uriString] = LspDocument{.uri = uriString, .text = text};

                        // compute diagnostics asynchronously using a detached thread and send PublishDiagnostics
                        std::ignore = std::async(std::launch::async,
                                                 [ uri = std::move(uri), text = std::move(text), &messageHandler, rtl =
                                                     this->m_options.
                                                     stdlibDirectories]() {
                                                     const auto messages =
                                                             collectDiagnostics(rtl, uri, text);

                                                     for (const auto &[file,_]: messages) {
                                                         const auto diagnostics = buildDiagnosticsFromMessages(
                                                             messages, file);
                                                         lsp::notifications::TextDocument_PublishDiagnostics::Params
                                                                 params2
                                                                 {
                                                                     .uri = toUri(file), .diagnostics = diagnostics
                                                                 };
                                                         messageHandler.sendNotification<
                                                             lsp::notifications::TextDocument_PublishDiagnostics>(
                                                             std::move(params2));
                                                     }
                                                 });
                    })
                .add<lsp::notifications::TextDocument_DidChange>(
                    [&](lsp::notifications::TextDocument_DidChange::Params &&params) {
                        const auto &uri = params.textDocument.uri;
                        // content changes: for Full sync the first change contains whole text
                        std::string text;
                        if (!params.contentChanges.empty()) {
                            text = std::get<lsp::TextDocumentContentChangeEvent_Text>(
                                params.contentChanges.front()).text;
                        }
                        m_openDocuments[uri.toString()] = LspDocument{.uri = uri.toString(), .text = text};

                        // async diagnostics
                        std::ignore = std::async(std::launch::async,
                                                 [ uri = uri, text = std::move(text), &messageHandler, rtl = this->
                                                     m_options.stdlibDirectories]() {
                                                     const auto messages =
                                                             collectDiagnostics(rtl, uri, text);
                                                     for (const auto &file: messages | std::views::keys) {
                                                         const auto diagnostics = buildDiagnosticsFromMessages(
                                                             messages, file);
                                                         lsp::notifications::TextDocument_PublishDiagnostics::Params
                                                                 params2
                                                                 {
                                                                     .uri = toUri(file), .diagnostics = diagnostics
                                                                 };
                                                         messageHandler.sendNotification<
                                                             lsp::notifications::TextDocument_PublishDiagnostics>(
                                                             std::move(params2));
                                                     }
                                                 }
                        );
                    })
                .add<lsp::notifications::TextDocument_DidClose>(
                    [&](lsp::notifications::TextDocument_DidClose::Params &&params) {
                        m_openDocuments.erase(params.textDocument.uri.toString());
                    })
                .add<lsp::requests::TextDocument_Definition>(
                    [&](lsp::requests::TextDocument_Definition::Params &&params) {
                        return findDefinition(params);
                    })
                .add<lsp::requests::TextDocument_Diagnostic>(
                    [&](lsp::requests::TextDocument_Diagnostic::Params &&params) {
                        auto diagnosticsReport = lsp::requests::TextDocument_Diagnostic::Result();
                        const auto &uri = params.textDocument.uri;
                        std::cerr << "Diagnostics requested for uri: " << uri.toString() << "\n";
                        // content changes: for Full sync the first change contains whole text
                        const std::string text = m_openDocuments.at(uri.toString()).text;


                        const auto messages = collectDiagnostics(this->m_options.stdlibDirectories, uri,
                                                                 text);
                        const auto diagnostics = buildDiagnosticsFromMessages(
                            messages, std::string(uri.path()));
                        auto fullDiagnosticsReport = lsp::RelatedFullDocumentDiagnosticReport{};
                        fullDiagnosticsReport.items = diagnostics;
                        std::cerr << "Found " << diagnostics.size() << " diagnostics\n";
                        diagnosticsReport = fullDiagnosticsReport;
                        return diagnosticsReport;
                    }).add<lsp::requests::Workspace_Diagnostic>(
                    [&](lsp::requests::Workspace_Diagnostic::Params &&params) {
                        auto diagnosticsReport = lsp::requests::Workspace_Diagnostic::Result();
                        std::map<lsp::Uri, lsp::WorkspaceFullDocumentDiagnosticReport> relatedDocuments;
                        for (const auto &doc: params.previousResultIds) {
                            const auto &uri = doc.uri;
                            std::cerr << "Workspace diagnostics requested for uri: " << uri.toString() << "\n";
                            // content changes: for Full sync the first change contains whole text
                            std::string text;
                            text = m_openDocuments.at(uri.toString()).text;
                            const auto messages = collectDiagnostics(this->m_options.stdlibDirectories, uri,
                                                                     text);
                            const auto diagnostics = buildDiagnosticsFromMessages(
                                messages, std::string(uri.path()));
                            auto report = lsp::WorkspaceFullDocumentDiagnosticReport{
                                .uri = uri
                            };
                            report.items = diagnostics;
                            diagnosticsReport.items.emplace_back(report);
                        }

                        return diagnosticsReport;
                    })
                .add<lsp::requests::TextDocument_Completion>(
                    [&](lsp::requests::TextDocument_Completion::Params &&item) {
                        return findCompletions(item);
                    });

        // Main loop
        while (running) {
            try {
                messageHandler.processIncomingMessages();
            } catch (const lsp::Error &e) {
                std::cerr << "LSP Exception: " << e.what() << "\n";
                std::cerr << "LSP Exception code: " << e.code() << "\n";
                if (e.data())
                    std::cerr << "LSP Exception data: " << e.data().value().string() << "\n";
            }
        }
    } catch (const lsp::Error &e) {
        std::cerr << "LSP Exception: " << e.what() << "\n";
        std::cerr << "LSP Exception code: " << e.code() << "\n";
        if (e.data())
            std::cerr << "LSP Exception data: " << e.data().value().string() << "\n";
    } catch (const std::exception &e) {
        std::cerr << "LSP ERROR: " << e.what() << std::endl;
    }
}

bool tokenInRange(const Token &token, size_t line, size_t character) {
    return token.source_location.row == line + 1 && character + 1 >= token.source_location.col &&
           character + 1 <= token.source_location.col + token.source_location.num_bytes;
}

lsp::requests::TextDocument_Completion::Result LanguageServer::findCompletions(
    const lsp::requests::TextDocument_Completion::Params &params) {
    auto result = lsp::requests::TextDocument_Completion::Result();
    // auto completionItem = lsp::CompletionItem{
    //     .label = "HelloWorld",
    //     .kind = lsp::CompletionItemKind::Function,
    //     .detail = "Example completion item",
    //     .documentation = lsp::MarkupContent{
    //         .kind = lsp::MarkupKind::Markdown,
    //         .value = "This is an example completion item provided by Zeus LSP."
    //     }
    // };
    //
    auto uri = params.textDocument.uri.toString();
    auto document = m_openDocuments.at(uri);

    auto completionList = lsp::CompletionList{
        .isIncomplete = true,
        .items = std::vector<lsp::CompletionItem>{}
    };
    result = completionList;
    auto size = (params.context.value().triggerCharacter) ? params.context.value().triggerCharacter.value().size() : 0;
    auto tokens = lexer::lex_file(uri, document.text);
    std::optional<Token> foundToken = std::nullopt;
    for (auto &token: tokens) {
        if (tokenInRange(token, params.position.line, params.position.character - size)) {
            foundToken = token;
            break;
        }
    }
    if (!foundToken.has_value()) {
        std::cerr << "No token found at position\n";
        return result;
    }
    auto parseResult = parser::parse_tokens(tokens);
    modules::include_modules(this->m_options.stdlibDirectories, parseResult);
    types::TypeCheckResult typeCheckResult;
    types::type_check(parseResult.module, typeCheckResult);

    auto resultPair = parseResult.module->getNodeByToken(foundToken.value());
    if (resultPair) {
        auto [parent, node] = resultPair.value();
        if (auto varAccess = dynamic_cast<const ast::VariableAccess *>(node)) {
            auto varName = varAccess->expressionToken();
            if (varAccess->expressionType()) {
                auto varType = varAccess->expressionType().value();
                if (auto structType = std::dynamic_pointer_cast<types::StructType>(varType)) {
                    for (const auto &field: structType->fields()) {
                        lsp::CompletionItem item;
                        item.label = field.name;
                        item.kind = lsp::CompletionItemKind::Field;
                        item.detail = "Field of struct " + structType->name();
                        completionList.items.push_back(std::move(item));
                    }
                    for (auto &method: structType->methods()) {
                        lsp::CompletionItem item;
                        item.label = method->functionName();
                        item.kind = lsp::CompletionItemKind::Method;
                        item.detail = structType->name() + "." + method->functionSignature();
                        completionList.items.push_back(std::move(item));
                    }
                }
            } else {
                std::cerr << "Variable " << varName.lexical() << " has no type information\n";
            }
        } else if (auto fieldAccess = dynamic_cast<const ast::FieldAccess *>(node)) {
            if (fieldAccess->expressionType()) {
                if (auto structType = std::dynamic_pointer_cast<types::StructType>(
                    fieldAccess->expressionType().value())) {
                    for (const auto &field: structType->fields()) {
                        lsp::CompletionItem item;
                        item.label = field.name;
                        item.kind = lsp::CompletionItemKind::Field;
                        item.detail = "Field of struct " + structType->name();
                        completionList.items.push_back(std::move(item));
                    }
                    for (auto &method: structType->methods()) {
                        lsp::CompletionItem item;
                        item.label = method->functionName();
                        item.kind = lsp::CompletionItemKind::Method;
                        item.detail = structType->name() + "." + method->functionSignature();
                        completionList.items.push_back(std::move(item));
                    }
                }
            } else {
                std::cerr << "Field access has no struct type information\n";
            }
        }
    } else {
        std::cerr << "No node found for token " << foundToken.value().lexical() << "\n";
    }
    std::cerr << "Found token: " << foundToken.value().lexical() << "\n";
    std::cerr << "Providing " << completionList.items.size() << " completion items\n";
    result = completionList;

    return result;
}

lsp::requests::TextDocument_Definition::Result LanguageServer::findDefinition(
    const lsp::requests::TextDocument_Definition::Params &params) {
    auto result = lsp::requests::TextDocument_Definition::Result();
    auto uri = params.textDocument.uri.toString();
    auto document = m_openDocuments.at(uri);
    auto tokens = lexer::lex_file(uri, document.text);
    std::optional<Token> foundToken = std::nullopt;
    for (auto &token: tokens) {
        if (tokenInRange(token, params.position.line, params.position.character)) {
            foundToken = token;
            break;
        }
    }
    if (!foundToken.has_value()) {
        std::cerr << "No token found at position\n";
        return result;
    }
    auto parseResult = parser::parse_tokens(tokens);
    modules::include_modules(this->m_options.stdlibDirectories, parseResult);
    types::TypeCheckResult typeCheckResult;
    types::type_check(parseResult.module, typeCheckResult);

    auto resultPair = parseResult.module->getNodeByToken(foundToken.value());
    if (resultPair) {
        std::cerr << "Found node for token " << foundToken.value().lexical() << "\n";
        auto [parent, node] = resultPair.value();
        if (auto varAccess = dynamic_cast<const ast::VariableAccess *>(node)) {
            auto varName = varAccess->expressionToken();
            if (parent) {
                if (auto function = dynamic_cast<ast::FunctionDefinition *>(parent)) {
                    if (auto varDefinition = function->getVariableDefinition(varName)) {
                        std::cerr << "Found variable definition for " << varName.lexical() << "\n";
                        lsp::Location location;
                        location.uri = toUri(
                            varDefinition.value()->expressionToken().source_location.filename);
                        location.range.start.line = static_cast<int>(
                                                        varDefinition.value()->expressionToken().source_location.row) -
                                                    1;
                        location.range.start.character = static_cast<int>(
                                                             varDefinition.value()->expressionToken().source_location.
                                                             col) - 1;
                        location.range.end.line = static_cast<int>(
                                                      varDefinition.value()->expressionToken().source_location.row) - 1;
                        location.range.end.character = static_cast<int>(
                                                           varDefinition.value()->expressionToken().source_location.col
                                                           +
                                                           varDefinition.value()->expressionToken().source_location.
                                                           num_bytes) - 1;
                        result.emplace(std::move(location));
                    }
                    for (const auto &param: function->args()) {
                        if (param.name.lexical() == varName.lexical()) {
                            std::cerr << "Found argument definition for " << varName.lexical() << "\n";
                            lsp::Location location;
                            location.uri = toUri(param.name.source_location.filename);
                            location.range.start.line = static_cast<int>(
                                                            param.name.source_location.row) - 1;
                            location.range.start.character = static_cast<int>(
                                                                 param.name.source_location.col) - 1;
                            location.range.end.line = static_cast<int>(
                                                          param.name.source_location.row) - 1;
                            location.range.end.character = static_cast<int>(
                                                               param.name.source_location.col +
                                                               param.name.source_location.num_bytes) - 1;
                            result.emplace(std::move(location));
                        }
                    }
                }
            }
        } else if (auto funcCall = dynamic_cast<const ast::FunctionCallNode *>(node)) {
            auto funcName = funcCall->functionName();
            std::cerr << "Found function " << funcName << "\n";
            auto funcResult = parseResult.module->findFunctionsByName(funcCall->modulePathName(), funcName);
            if (!funcResult)
                return result;
            if (const auto &[module, func] = funcResult.value(); func) {
                if (func->functionName() == funcName) {
                    std::cerr << "Found function definition for " << funcName << "\n";
                    lsp::Location location;
                    location.uri = toUri(func->expressionToken().source_location.filename);
                    location.range.start.line = static_cast<int>(
                                                    func->expressionToken().source_location.row) - 1;
                    location.range.start.character = static_cast<int>(
                                                         func->expressionToken().source_location.col) - 1;
                    location.range.end.line = static_cast<int>(
                                                  func->expressionToken().source_location.row) - 1;
                    location.range.end.character = static_cast<int>(
                                                       func->expressionToken().source_location.col +
                                                       func->expressionToken().source_location.num_bytes) - 1;
                    result.emplace(std::move(location));
                }
            }
        } else if (auto methodCall = dynamic_cast<const ast::MethodCallNode *>(node)) {
            auto funcName = methodCall->functionName();
            auto instanceType = methodCall->instanceNode()->expressionType();
            if (!instanceType) {
                std::cerr << "Method call instance has no type information\n";
                return result;
            }
            if (auto refType = std::dynamic_pointer_cast<types::ReferenceType>(instanceType.value())) {
                if (auto refType = std::dynamic_pointer_cast<types::ReferenceType>(instanceType.value())) {
                    instanceType = refType->baseType();
                }
            }
            if (auto structType = std::dynamic_pointer_cast<types::StructType>(instanceType.value())) {
                bool methodFound = false;
                for (const auto &method: structType->methods()) {
                    if (method->functionName() == funcName) {
                        std::cerr << "Found method definition for " << funcName << " in struct " << structType->name()
                                << "\n";
                        lsp::Location location;
                        location.uri = toUri(
                            method->expressionToken().source_location.filename);
                        location.range.start.line = static_cast<int>(
                                                        method->expressionToken().source_location.row) - 1;
                        location.range.start.character = static_cast<int>(
                                                             method->expressionToken().source_location.col) - 1;
                        location.range.end.line = static_cast<int>(
                                                      method->expressionToken().source_location.row) - 1;
                        location.range.end.character = static_cast<int>(
                                                           method->expressionToken().source_location.col
                                                           +
                                                           method->expressionToken().source_location.
                                                           num_bytes) - 1;
                        result.emplace(std::move(location));
                        methodFound = true;
                        break;
                    }
                }
                if (methodFound)
                    return result;
            }
        }
    }

    return result;
}
