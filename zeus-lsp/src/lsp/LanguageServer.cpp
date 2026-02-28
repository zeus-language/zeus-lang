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
    }
    return lsp::FileUri::fromPath(filePath);
}

static std::vector<lsp::Diagnostic> buildDiagnosticsFromMessages(
    const std::map<std::string, std::vector<parser::ParserMessasge> > &messages,
    const std::string &targetUri) {
    std::vector<lsp::Diagnostic> diagnostics;

    const auto it = messages.find(targetUri);
    if (it == messages.end())
        return diagnostics;

    for (const auto &[outputType, token, message]: it->second) {
        lsp::Diagnostic diag;
        // adjust to 0-based indexing

        diag.range.start.line = static_cast<int>(token.source_location.row) - 1;
        diag.range.start.character = static_cast<int>(token.source_location.col) - 1;
        diag.range.end.line = static_cast<int>(token.source_location.row) - 1;
        diag.range.end.character = static_cast<int>(token.source_location.col + token.source_location.num_bytes) - 1;
        diag.severity = mapOutputTypeToSeverity(outputType);
        diag.message = message;
        diag.source = std::string("zeus");
        diag.relatedInformation = std::vector<lsp::DiagnosticRelatedInformation>{
            lsp::DiagnosticRelatedInformation{
                .location = lsp::Location{
                    .uri = toUri(token.source_location.filename),
                    .range = diag.range
                },
                .message = message
            }
        };
        diagnostics.push_back(std::move(diag));
    }

    return diagnostics;
}


static std::map<std::string, std::vector<parser::ParserMessasge> > collectDiagnostics(
    const std::vector<std::filesystem::path> &rtlDirectories,
    modules::ModuleCache& moduleCache,
    const lsp::Uri &uri,
    const std::string &text) {
    std::map<std::string, std::vector<parser::ParserMessasge> > errorsMap;

    const  std::filesystem::path file_path(uri.path());
    const std::filesystem::path parentDir = file_path.parent_path();
    const auto tokens = lexer::lex_file(std::string(file_path), text);
    auto result = parser::parse_tokens(tokens);
    std::vector<std::filesystem::path> includeDirs;
    for (auto &dir: rtlDirectories) {
        if (std::filesystem::exists(dir)) {
            includeDirs.push_back(dir);
        }
    }
    includeDirs.push_back(parentDir);
    for (auto &tmpDir : includeDirs) {
        std::cerr << "Warning: Include directory '" << tmpDir.string() << "' !\n";

    }
    modules::include_modules(includeDirs,moduleCache, result);

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

/**
 *
 * @param type
 *                                 "namespace", "enum", "interface", "struct", "typeParameter",
                                "parameter", "variable", "property", "enumMember", "event", "function", "method",
                                "macro", "keyword", "modifier", "comment", "string", "number", "regexp", "operator"
 * @return
 */
std::optional<int> mapTokenType(const Token::Type type) {
    switch (type) {
        case Token::Type::KEYWORD:
            return 13;
        case Token::Type::STRING:
        case Token::Type::RAW_STRING:
        case Token::Type::UNCLOSED_STRING:
        case Token::Type::UNCLOSED_RAW_STRING:
        case Token::Type::INTERPOLATED_STRING:
        case Token::Type::INTERPOLATION_START:
        case Token::Type::INTERPOLATION_END:
        case Token::Type::CHAR:
            return 16;
        case Token::Type::NUMBER:
        case Token::Type::FLOAT_NUMBER:
            return 17;
        case Token::Type::LINE_COMMENT:
        case Token::Type::BLOCK_COMMENT:
            return 15;
        case Token::Type::ANNOTATION:
            return 20;
        case Token::Type::IDENTIFIER:
        default:
            return std::nullopt;
    }
}


void processMultiLineToken(const Token &token, int tokenType,
                           std::vector<uint32_t> &semanticTokens,
                           size_t &lastRow, size_t &lastCol) {
    const std::string &text = token.source_location.text();

    // Count newlines in token to determine if it's multi-line
    size_t newlineCount = 0;

    for (size_t i = 0; i < text.length(); ++i) {
        if (text[i] == '\n') {
            newlineCount++;
            break;
        }
    }

    if (newlineCount == 0) {
        // Single-line token - simple case
        if (lastRow != token.source_location.row - 1) {
            lastCol = 0;
        }
        // std::cerr << "type of token: " << tokenType << "\n";
        // std::cerr << "Emitting token at row " << token.source_location.row - 1 << ", col "
        //         << token.source_location.col - 1 << ", length " << token.source_location.num_bytes << "\n";

        semanticTokens.push_back(
            static_cast<uint32_t>(token.source_location.row - lastRow - 1)); // line delta
        semanticTokens.push_back(
            static_cast<uint32_t>(token.source_location.col - lastCol - 1)); // startChar delta
        semanticTokens.push_back(static_cast<uint32_t>(token.source_location.num_bytes)); // length
        semanticTokens.push_back(tokenType); // tokenType
        semanticTokens.push_back(0); // tokenModifiers

        lastRow = token.source_location.row - 1;
        lastCol = token.source_location.col - 1;
    } else {
        // Multi-line token
        size_t startCol = token.source_location.col - 1;
        size_t currentRow = token.source_location.row - 1;

        size_t segmentStart = 0;
        for (size_t i = 0; i < text.length(); ++i) {
            if (i == text.length() - 1 || text[i] == '\n') {
                size_t segmentLength = i - segmentStart;
                if (segmentLength > 0) {
                    // Emit token for this segment
                    if (lastRow != currentRow) {
                        lastCol = 0;
                    }

                    semanticTokens.push_back(
                        static_cast<uint32_t>(currentRow - lastRow)); // line delta
                    semanticTokens.push_back(
                        static_cast<uint32_t>(startCol - lastCol)); // startChar delta
                    semanticTokens.push_back(static_cast<uint32_t>(segmentLength)); // length
                    semanticTokens.push_back(tokenType); // tokenType
                    semanticTokens.push_back(0); // tokenModifiers

                    lastRow = currentRow;
                    lastCol = startCol + segmentLength;
                }

                // Move to next line
                currentRow++;
                startCol = 0;
                segmentStart = i + 1;
            }
        }
    }
}

LanguageServer::LanguageServer(lsp::LspOptions options) : m_options(std::move(options)), m_moduleCache(true) {
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
                        .triggerCharacters = std::vector<std::string>{".", ":"},
                        .resolveProvider = true,

                    };
                    result.capabilities.semanticTokensProvider = lsp::SemanticTokensOptions{
                        .legend = lsp::SemanticTokensLegend{
                            .tokenTypes = {
                                "namespace", "enum", "interface", "struct", "typeParameter",
                                "parameter", "variable", "property", "enumMember", "event", "function", "method",
                                "macro", "keyword", "modifier", "comment", "string", "number", "regexp", "operator",
                                "decorator"
                            },
                            .tokenModifiers = {
                                "declaration", "definition", "readonly", "static", "deprecated", "abstract",
                                "async", "modification", "documentation", "defaultLibrary"
                            }
                        },
                        .range = false,
                        .full = true
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
                        const auto uriString = params.textDocument.uri.toString();
                        auto text = params.textDocument.text;
                        m_openDocuments[uriString] = LspDocument{.uri = uriString, .text = text};

                        // compute diagnostics asynchronously using a detached thread and send PublishDiagnostics
                        std::ignore = std::async(std::launch::async,
                                                 [ uri = std::move(uri), text = std::move(text), &messageHandler, rtl =
                                                     this->m_options.stdlibDirectories, &cache = m_moduleCache
                                                     ]() {
                                                     const auto messages =
                                                             collectDiagnostics(rtl,cache, uri, text);

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
                        auto* cache = &m_moduleCache;
                        // async diagnostics
                        std::ignore = std::async(std::launch::async,
                                                 [ uri = uri, text = std::move(text), &messageHandler, rtl = this->
                                                     m_options.stdlibDirectories,  cache]() {
                                                     const auto messages =
                                                             collectDiagnostics(rtl,*cache, uri, text);
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

                        try {
                            const auto messages = collectDiagnostics(this->m_options.stdlibDirectories,m_moduleCache, uri,
                                                                     text);
                            const auto diagnostics = buildDiagnosticsFromMessages(
                                messages, std::string(uri.path()));
                            auto fullDiagnosticsReport = lsp::RelatedFullDocumentDiagnosticReport{};
                            fullDiagnosticsReport.items = diagnostics;
                            std::cerr << "Found " << diagnostics.size() << " diagnostics\n";
                            diagnosticsReport = fullDiagnosticsReport;
                        }catch (const std::exception &e) {
                            std::cerr << "EXCEPTION: " << e.what() << std::endl;
                        }
                        return diagnosticsReport;
                    }).add<lsp::requests::Workspace_Diagnostic>(
                    [&](lsp::requests::Workspace_Diagnostic::Params &&params) {
                        auto diagnosticsReport = lsp::requests::Workspace_Diagnostic::Result();
                        std::map<lsp::Uri, lsp::WorkspaceFullDocumentDiagnosticReport> relatedDocuments;
                        for (const auto &[uri, value]: params.previousResultIds) {
                            std::cerr << "Workspace diagnostics requested for uri: " << uri.toString() << "\n";
                            // content changes: for Full sync the first change contains whole text
                            std::string text;
                            text = m_openDocuments.at(uri.toString()).text;
                            const auto messages = collectDiagnostics(this->m_options.stdlibDirectories,m_moduleCache, uri,
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
                    }).add<lsp::requests::TextDocument_SemanticTokens_Full>(
                    [&](lsp::requests::TextDocument_SemanticTokens_Full::Params &&params) {
                        auto result = lsp::requests::TextDocument_SemanticTokens_Full::Result();
                        const auto uri = params.textDocument.uri.toString();
                        const auto [documentUri, text] = m_openDocuments.at(uri);
                        const auto tokens = lexer::lex_file(uri, text, false);
                        auto semanticTokens = lsp::SemanticTokens{
                            .data = std::vector<uint32_t>{}
                        };
                        size_t lastRow = 0;
                        size_t lastCol = 0;
                        for (const auto &token: tokens) {
                            if (auto tokenType = mapTokenType(token.type)) {
                                // Use helper function to properly handle multi-line tokens
                                // (especially block comments that span multiple lines)
                                processMultiLineToken(token, tokenType.value(), semanticTokens.data,
                                                      lastRow, lastCol);
                            }
                        }
                        result.emplace(semanticTokens);
                        return result;
                    });

        // Main loop
        while (running) {
            try {
                messageHandler.processIncomingMessages();
            } catch (const lsp::MessageError &e) {
                std::cerr << "LSP Exception: " << e.what() << "\n";
                std::cerr << "LSP Exception code: " << e.code() << "\n";
                if (e.data())
                    std::cerr << "LSP Exception data: " << e.data().value().string() << "\n";
            }catch (const std::exception &e) {
                std::cerr << "EXCEPTION: " << e.what() << std::endl;
            }
        }
    } catch (const lsp::MessageError &e) {
        std::cerr << "LSP Exception: " << e.what() << "\n";
        std::cerr << "LSP Exception code: " << e.code() << "\n";
        if (e.data())
            std::cerr << "LSP Exception data: " << e.data().value().string() << "\n";
    } catch (const std::exception &e) {
        std::cerr << "LSP ERROR: " << e.what() << std::endl;

    }
}

bool tokenInRange(const Token &token,const size_t line,const size_t character) {
    return token.source_location.row == line + 1 && character + 1 >= token.source_location.col &&
           character + 1 <= token.source_location.col + token.source_location.num_bytes;
}
void addToCompletionListIfMatches(lsp::CompletionItem item,
                                  lsp::CompletionList &completionList) {
    const auto containsItem = std::ranges::find(completionList.items, item.label,
                                               &lsp::CompletionItem::label);
    if (containsItem == completionList.items.end()) {
        completionList.items.push_back(std::move(item));
    }
}

void addCompletionItemForModule(const parser::Module * module, const char * token,  lsp::CompletionList & completions) {
    auto moduleName = module->modulePath().back().lexical();
    if (module->aliasName.has_value()) {
        moduleName = module->aliasName.value();
    }
    auto containsName = moduleName.find(token);
    if (containsName != std::string::npos) {
        lsp::CompletionItem item;
        item.label = moduleName;
        item.insertText = moduleName + "::";
        item.insertTextMode = lsp::InsertTextMode::AdjustIndentation;
        item.kind = lsp::CompletionItemKind::Module;
        item.detail = "Module " + moduleName;
        addToCompletionListIfMatches(std::move(item), completions);
    }

}
void addCompletionItemForFunction(const ast::FunctionDefinitionBase *function,std::string nsPrefix, const std::string& token,
                                  lsp::CompletionList &completions) {
    const auto definedName = function->functionName();
    const auto containsName = definedName.find(token);
    if (containsName != std::string::npos) {
        lsp::CompletionItem item;
        item.label = function->functionName();
        auto insertText = function->functionName() + "(";
        for (size_t i = 0; i < function->args().size(); ++i) {
            insertText += "${" + std::to_string(i + 1) + ":" + function->args()[i].name.lexical() + "}";
            if (i < function->args().size() - 1) {
                insertText += ", ";
            }
        }
        insertText += ")";
        item.insertText = insertText;
        item.insertTextMode = lsp::InsertTextMode::AdjustIndentation;
        item.insertTextFormat = lsp::InsertTextFormat::Snippet;
        if (auto funcDef = dynamic_cast<const ast::FunctionDefinition *>(function)) {
            if (funcDef->isMethod()) {
                item.kind = lsp::CompletionItemKind::Method;
            } else
            {
                item.kind = lsp::CompletionItemKind::Function;
            }
        } else
        {
            item.kind = lsp::CompletionItemKind::Function;
        }

        item.detail = nsPrefix+function->functionSignature(false);
        addToCompletionListIfMatches(std::move(item), completions);
    }
}



bool findMemberCompletion(lsp::requests::TextDocument_Completion::Result &result, lsp::CompletionList completionList, std::optional<Token> foundToken,const parser::ParseResult& parseResult)  {
    if (auto resultPair = parseResult.module->getNodeByToken(foundToken.value())) {
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
                        item.insertText = method->functionName() + "()";
                        item.insertTextMode = lsp::InsertTextMode::AdjustIndentation;
                        item.kind = lsp::CompletionItemKind::Method;
                        item.detail = structType->name() + "." + method->functionSignature(false);
                        completionList.items.push_back(std::move(item));
                    }
                }
                result = completionList;
                return true;
            }
            if (auto functionDef = dynamic_cast<ast::FunctionDefinition *>(parent)) {
                for (const auto &statement: functionDef->statements()) {
                    if (auto varDefinition = dynamic_cast<ast::VariableDeclaration *>(statement.get())) {
                        auto definedName = varDefinition->expressionToken().lexical();
                        auto containsName = definedName.find(varName.lexical());
                        if (containsName != std::string::npos) {
                            lsp::CompletionItem item;
                            item.label = definedName;
                            item.kind = (varDefinition->constant())
                                            ? lsp::CompletionItemKind::Constant
                                            : lsp::CompletionItemKind::Variable;
                            if (varDefinition->expressionType()) {
                                item.detail = varDefinition->expressionType().value()->name();
                            } else {
                                item.detail = "Unknown type";
                            }
                            completionList.items.push_back(std::move(item));
                        }
                    }
                }
            } else {
                std::cerr << "Variable " << varName.lexical() << " has no type information\n";
            }
        } else if (auto fieldAccess = dynamic_cast<const ast::FieldAccess *>(node)) {
            if (fieldAccess->expressionType()) {
                if (auto structType = std::dynamic_pointer_cast<types::StructType>(
                    fieldAccess->expressionType().value())) {
                    for (const auto &[visibility,type, name]: structType->fields()) {
                        lsp::CompletionItem item;
                        item.label = name;
                        item.kind = lsp::CompletionItemKind::Field;
                        item.detail = "Field of struct " + structType->name();
                        completionList.items.push_back(std::move(item));
                    }
                    for (auto &method: structType->methods()) {
                        lsp::CompletionItem item;
                        item.label = method->functionName();
                        item.insertText = method->functionName() + "()";
                        item.insertTextMode = lsp::InsertTextMode::AdjustIndentation;
                        item.kind = lsp::CompletionItemKind::Method;
                        item.detail = structType->name() + "." + method->functionSignature(false);
                        completionList.items.push_back(std::move(item));
                    }
                    result = completionList;
                    return true;
                }
            } else {
                std::cerr << "Field access has no struct type information\n";
            }
        }
    } else {
        std::cerr << "No node found for token " << foundToken.value().lexical() << "\n";
    }
    return false;
}

std::vector<Token> build_path_from_token_reverse(std::vector<Token> tokens, std::optional<Token> &foundToken,bool& isUseCompletion)  {
    std::vector<Token> pathTokens;
    while (true) {
        pathTokens.push_back(foundToken.value());
        auto previousTokenIt = std::find(tokens.rbegin(), tokens.rend(),foundToken.value());
        if (previousTokenIt != tokens.rend()) {
            std::optional<Token> previousToken = std::nullopt;
            for (auto it = previousTokenIt + 1; it != tokens.rend(); ++it) {
                if ( it->type != Token::LINE_COMMENT &&
                     it->type != Token::BLOCK_COMMENT &&
                     it->type != Token::NS_SEPARATOR) {
                    previousToken = *it;
                    break;
                }
            }

            if (previousToken.has_value()) {
                if (previousToken->type != Token::Type::IDENTIFIER && previousToken->type != Token::Type::NS_SEPARATOR) {
                    if (previousToken->type == Token::Type::KEYWORD &&
                        previousToken->lexical() == "use") {
                        isUseCompletion = true;
                        std::cerr<< "Found use statement for completion\n";
                    }
                    break;
                }
                foundToken = previousToken;
            } else {
                break;
            }
        } else {
            break;
        }
    }
    std::ranges::reverse(pathTokens);
    return pathTokens;
}

lsp::requests::TextDocument_Completion::Result LanguageServer::findCompletions(
    const lsp::requests::TextDocument_Completion::Params &params)  {
    auto result = lsp::requests::TextDocument_Completion::Result();

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
        std::cerr << "No token found at position for completion\n";
        return result;
    }
    auto parseResult = parser::parse_tokens(tokens);

    modules::include_modules(this->m_options.stdlibDirectories,m_moduleCache, parseResult);
    types::TypeCheckResult typeCheckResult;
    types::type_check(parseResult.module, typeCheckResult);
    if (foundToken->type == Token::NS_SEPARATOR) {
        // handle member completions
        auto previousTokenIt = std::find(tokens.rbegin(), tokens.rend(),foundToken.value());
        if (previousTokenIt != tokens.rend()) {
            std::optional<Token> previousToken = std::nullopt;
            for (auto it = previousTokenIt + 1; it != tokens.rend(); ++it) {
                if ( it->type != Token::LINE_COMMENT &&
                    it->type != Token::BLOCK_COMMENT) {
                    previousToken = *it;
                    break;
                }
            }
            if (previousToken.has_value()) {
                std::cerr << "Found previous token for namespace separator: " << previousToken->lexical()
                          << "\n";
                foundToken = previousToken;
            } else {
                std::cerr << "No previous token found for namespace separator\n";
                return result;
            }
        } else {
            std::cerr << "No previous token found for namespace separator\n";
            return result;
        }
        bool isUseCompletion = false;

        std::vector<Token> pathTokens = build_path_from_token_reverse(tokens, foundToken, isUseCompletion);
        std::cerr<< "is use completion: " << isUseCompletion << "\n";

        auto completionResult = parseResult.module->findModulesByPathStart(
            pathTokens);
        for (const auto &mod: completionResult) {

            addCompletionItemForModule(mod.get(), "", completionList);
            if (!isUseCompletion) {
                for (const auto &function: mod->functions) {
                    addCompletionItemForFunction(function.get(),mod->modulePathName(), "", completionList);
                }
            }
        }
        if (isUseCompletion) {
            completionResult = m_moduleCache.findModulesByPathStart(pathTokens);
            for (const auto &mod: completionResult) {

                addCompletionItemForModule(mod.get(), "", completionList);
            }
        }
        result = completionList;
        return result;
    }

    if (findMemberCompletion(result, completionList, foundToken, parseResult)) return result;
    for (auto &function: parseResult.module->functions) {
        addCompletionItemForFunction(function.get(),"", foundToken.value().lexical(), completionList);
    }
    for (auto &importModule: parseResult.module->modules) {
        for (auto &function: importModule->functions) {
            addCompletionItemForFunction(function.get(),importModule->modulePathName(), foundToken.value().lexical(), completionList);
        }
    }
    for (const auto& keyword: lexer::keywords()) {
        if (auto containsName = keyword.find(foundToken.value().lexical()); containsName != std::string::npos) {
            lsp::CompletionItem item;
            item.label = keyword;
            item.kind = lsp::CompletionItemKind::Keyword;
            completionList.items.push_back(std::move(item));
        }
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
    const auto [documentUri, text] = m_openDocuments.at(uri);
    auto tokens = lexer::lex_file(uri, text);
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
    modules::include_modules(this->m_options.stdlibDirectories,m_moduleCache, parseResult);
    types::TypeCheckResult typeCheckResult;
    types::type_check(parseResult.module, typeCheckResult);

    if (auto resultPair = parseResult.module->getNodeByToken(foundToken.value())) {
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
                    std::cerr << "Found function definition for " << funcName <<"\n"<<func->expressionToken().source_location.filename<< "\n";
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
                instanceType = refType->baseType();
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
