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

#include "ast/FunctionCallNode.h"
#include "ast/FunctionDefinition.h"
#include "ast/VariableAccess.h"
#include "lexer/Lexer.h"
#include "parser/module.h"
#include "parser/Parser.h"
#include "types/TypeChecker.h"

using namespace std::string_view_literals;

static int mapOutputTypeToSeverity(const parser::OutputType output) {
    switch (output) {
        case parser::OutputType::ERROR:
            return static_cast<int>(lsp::DiagnosticSeverity::Error);
        case parser::OutputType::HINT:
            return static_cast<int>(lsp::DiagnosticSeverity::Hint);
        case parser::OutputType::WARN:
            return static_cast<int>(lsp::DiagnosticSeverity::Warning);
    }
    return static_cast<int>(lsp::DiagnosticSeverity::Information);
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
        diag.severity = static_cast<lsp::DiagnosticSeverity>(mapOutputTypeToSeverity(msg.outputType));
        diag.message = msg.message;
        diag.source = std::string("zeus");
        diagnostics.push_back(std::move(diag));
    }

    return diagnostics;
}

static std::map<std::string, std::vector<parser::ParserMessasge> > collectDiagnostics(
    const std::vector<std::filesystem::path> &rtlDirectories,
    const std::string &uri,
    const std::string &text) {
    std::map<std::string, std::vector<parser::ParserMessasge> > errorsMap;

    auto tokens = lexer::lex_file(uri, text);
    auto result = parser::parse_tokens(tokens);
    modules::include_modules(rtlDirectories, result);

    types::TypeCheckResult type_check_result;
    types::type_check(result.module, type_check_result);

    if (result.messages.empty() && type_check_result.messages.empty()) {
        errorsMap[uri] = {};
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
                    result.capabilities.positionEncoding = lsp::PositionEncodingKind::UTF16;
                    result.capabilities.textDocumentSync = lsp::TextDocumentSyncOptions{
                        .openClose = true,
                        .change = lsp::TextDocumentSyncKind::Full,
                        .save = true
                    };
                    result.capabilities.hoverProvider = false;
                    result.capabilities.definitionProvider = true;
                    result.capabilities.declarationProvider = true;

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
                        std::thread(
                            [this, uri = std::move(uri), text = std::move(text), &messageHandler, rtl = this->m_options.
                                stdlibDirectories]() mutable {
                                auto messages = collectDiagnostics(rtl, uri.toString(), text);
                                auto diagnostics = buildDiagnosticsFromMessages(messages, uri.toString());
                                lsp::notifications::TextDocument_PublishDiagnostics::Params params2{
                                    .uri = uri, .diagnostics = diagnostics
                                };
                                messageHandler.sendNotification<lsp::notifications::TextDocument_PublishDiagnostics>(
                                    std::move(params2));
                            }).detach();
                    })
                .add<lsp::notifications::TextDocument_DidChange>(
                    [&](lsp::notifications::TextDocument_DidChange::Params &&params) {
                        const auto &uri = params.textDocument.uri;
                        // content changes: for Full sync the first change contains whole text
                        std::string text;
                        if (!params.contentChanges.empty()) {
                            text = std::get<lsp::TextDocumentContentChangeEvent_Range_Text>(
                                params.contentChanges.front()).text;
                        }
                        m_openDocuments[uri.toString()] = LspDocument{.uri = uri.toString(), .text = text};

                        // async diagnostics
                        std::thread(
                            [this, uri = uri, text = std::move(text), &messageHandler, rtl = this->
                                m_options.stdlibDirectories]() mutable {
                                auto messages = collectDiagnostics(rtl, uri.toString(), text);
                                auto diagnostics = buildDiagnosticsFromMessages(messages, uri.toString());
                                lsp::notifications::TextDocument_PublishDiagnostics::Params params2{
                                    .uri = uri, .diagnostics = diagnostics
                                };
                                messageHandler.sendNotification<lsp::notifications::TextDocument_PublishDiagnostics>(
                                    std::move(params2));
                            }).detach();
                    })
                .add<lsp::notifications::TextDocument_DidClose>(
                    [&](lsp::notifications::TextDocument_DidClose::Params &&params) {
                        m_openDocuments.erase(params.textDocument.uri.toString());
                    })
                .add<lsp::requests::TextDocument_Definition>(
                    [&](lsp::requests::TextDocument_Definition::Params &&params) {
                        return findDefinition(params);
                    });

        // Main loop
        while (running) {
            messageHandler.processIncomingMessages();
        }
    } catch (const std::exception &e) {
        std::cerr << "LSP ERROR: " << e.what() << std::endl;
    }
}

bool tokenInRange(const Token &token, size_t line, size_t character) {
    return token.source_location.row == line + 1 && character + 1 >= token.source_location.col &&
           character + 1 <= token.source_location.col + token.source_location.num_bytes;
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

    auto resultPair = parseResult.module->getNodeByToken(foundToken.value());
    if (resultPair) {
        std::cerr << "Found node for token " << foundToken.value().lexical() << "\n";
        auto [parent, node] = resultPair.value();
        if (auto varAccess = dynamic_cast<const ast::VariableAccess *>(node)) {
            auto varName = varAccess->expressionToken().lexical();
            if (parent) {
                if (auto function = dynamic_cast<ast::FunctionDefinition *>(parent)) {
                    if (auto varDefinition = function->getVariableDefinition(varName)) {
                        std::cerr << "Found variable definition for " << varName << "\n";
                        lsp::Location location;
                        location.uri = lsp::FileUri::fromPath(
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
                        if (param.name.lexical() == varName) {
                            std::cerr << "Found argument definition for " << varName << "\n";
                            lsp::Location location;
                            location.uri = lsp::FileUri::fromPath(param.name.source_location.filename);
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
                    location.uri = lsp::FileUri::fromPath(func->expressionToken().source_location.filename);
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
        }
    }

    return result;
}
