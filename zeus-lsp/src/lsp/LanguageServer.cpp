//
// Created by stefan on 29.09.25.
//

#include "LanguageServer.h"
#include <future>
#include <iostream>
#include <string>
#include <vector>
#include <json/json.hpp>

#include "lexer/Lexer.h"
#include "parser/module.h"
#include "parser/Parser.h"
#include "types/TypeChecker.h"
using namespace std::string_view_literals;

nlohmann::json buildPosition(const size_t line, const size_t character) {
    nlohmann::json response;
    response["line"] = line - 1;
    response["character"] = character - 1;
    return response;
}

constexpr int mapOutputTypeToSeverity(const parser::OutputType output) {
    switch (output) {
        case parser::OutputType::ERROR:
            return 1;
        case parser::OutputType::HINT:
            return 4;
        case parser::OutputType::WARN:
            return 2;
    }
    return 0;
}

void sentDiagnostics(std::map<std::string, std::vector<parser::ParserMessasge> > messages) {
    if (messages.empty())
        return;

    try {
        // group messages by file
        using json = nlohmann::json;

        for (auto &[fileName, messsages]: messages) {
            json logMessages = nlohmann::json::array();
            for (const auto &[outputType, token, message]: messsages) {
                json logMessage;
                json range;
                range["start"] = buildPosition(token.source_location.row, token.source_location.col);
                range["end"] = buildPosition(token.source_location.row,
                                             token.source_location.col + token.source_location.num_bytes);
                logMessage["range"] = range;
                logMessage["severity"] = mapOutputTypeToSeverity(outputType);
                logMessage["message"] = message;
                json relatedInformations = nlohmann::json::array();
                json source;
                json location;
                location["uri"] = token.source_location.filename;
                json range2;
                range2["start"] = buildPosition(token.source_location.row, token.source_location.col);
                range2["end"] = buildPosition(token.source_location.row,
                                              token.source_location.col + token.source_location.num_bytes);
                location["range"] = range2;
                source["location"] = location;
                source["message"] = message;
                source["source"] = "wirthx";
                relatedInformations.push_back(source);

                logMessage["relatedInformation"] = relatedInformations;
                logMessages.push_back(std::move(logMessage));
            }
            json response;
            response["jsonrpc"] = "2.0";
            response["method"] = "textDocument/publishDiagnostics";
            json diagnosticsParams = nlohmann::json::array();
            json PublishDiagnosticsParams;
            PublishDiagnosticsParams["uri"] = fileName;
            PublishDiagnosticsParams["diagnostics"] = logMessages;
            diagnosticsParams.push_back(PublishDiagnosticsParams);

            response["params"] = diagnosticsParams;


            std::stringstream sstream;
            sstream << response;
            std::cerr << "TEST: " << sstream.str() << "\n";
            std::cout << "Content-Length: " << sstream.str().length() << "\r\n\r\n";
            std::cout << sstream.str();
        }
    } catch (nlohmann::json::exception &e) {
        std::cerr << "ERROR: sendDignostics: " << e.what() << "\n";
    }
}

void parseAndSendDiagnostics(std::vector<std::filesystem::path> rtlDirectories, std::string uri,
                             std::string text) {
    std::map<std::string, std::vector<parser::ParserMessasge> > errorsMap;

    auto tokens = lexer::lex_file(uri, text);
    std::filesystem::path filePath = uri;

    auto result = parser::parse_tokens(tokens);
    const auto nodes = modules::include_modules(rtlDirectories, result);

    types::TypeCheckResult type_check_result;
    types::type_check(nodes, type_check_result);


    if (result.messages.empty() && type_check_result.messages.empty()) {
        errorsMap[filePath.string()] = {};
    } else {
        for (auto &error: result.messages) {
            errorsMap[error.token.source_location.filename].push_back(error);
        }
        for (const auto &msg: type_check_result.messages) {
            errorsMap[msg.token.source_location.filename].push_back(msg);
        }
    }
    sentDiagnostics(errorsMap);
}

nlohmann::json buildError(const char *message, const int errorCode) {
    nlohmann::json error;
    error["code"] = errorCode;
    error["message"] = message;
    error["data"] = "An internal error occurred while processing the request.";
    return error;
}

LanguageServer::LanguageServer(lsp::LspOptions options) : m_options(std::move(options)) {
}

void LanguageServer::handleRequest() {
    while (true) {
        std::string commandString;
        getline(std::cin, commandString);
        auto length = std::atoi(commandString.substr(16).c_str());
        getline(std::cin, commandString); // empty line
        std::vector<char> buffer;
        buffer.resize(length);
        std::cin.read(&buffer[0], length);
        commandString = std::string(buffer.begin(), buffer.end());
        nlohmann::json json;
        std::cerr << "command: " << commandString << "\n";
        json = nlohmann::json::parse(commandString);

        auto method = json.value("method", std::string());
        bool hasId = false;
        std::string id;

        nlohmann::json response;
        response["jsonrpc"] = "2.0";

        auto jsonId = json.find("id");
        if (jsonId == json.end()) {
        } else if (jsonId->is_number_integer()) {
            response["id"] = json.value("id", 0);
            hasId = true;
        } else if (json.find("id")->is_string()) {
            response["id"] = json.value("id", "");
            hasId = true;
        }


        if (method == "shutdown") {
            return;
        }
        if (method == "initialize") {
            nlohmann::json capabilities;
            capabilities["documentHighlightProvider"] = false;
            capabilities["documentSymbolProvider"] = true;
            capabilities["colorProvider"] = false;
            nlohmann::json diagnosticProvider;
            diagnosticProvider["interFileDependencies"] = true;
            diagnosticProvider["workspaceDiagnostics"] = false;
            capabilities["diagnosticProvider"] = std::move(diagnosticProvider);
            nlohmann::json textDocumentSync;
            textDocumentSync["openClose"] = true;
            textDocumentSync["change"] = 1;
            capabilities["textDocumentSync"] = std::move(textDocumentSync);
            capabilities["declarationProvider"] = true;
            capabilities["definitionProvider"] = true;
            nlohmann::json result;
            result["capabilities"] = std::move(capabilities);

            nlohmann::json completionProvider;
            std::vector<std::string> triggerCharacters = {"."};
            completionProvider["triggerCharacters"] = triggerCharacters;
            result["completionProvider"] = std::move(completionProvider);

            nlohmann::json serverInfo;
            serverInfo["name"] = "zeusls";
            serverInfo["version"] = "0.1";
            result["serverInfo"] = std::move(serverInfo);
            response["result"] = std::move(result);
        }
        if (method == "initialized") {
            //response["result"] = nlohmann::json::object();
            continue;
        } else if (method == "textDocument/didOpen") {
            std::string pos = "start";
            try {
                auto params = json.at("params").get<nlohmann::json>();
                pos = "uri";
                auto uri = params["textDocument"]["uri"].get<std::string>(); //.("uri");
                pos = "text";
                auto text = params["textDocument"]["text"].get<std::string>();
                m_openDocuments[uri] =
                        LspDocument{.uri = uri, .text = text};
                response["result"] = nlohmann::json::object();
                std::ignore = std::async(std::launch::async, [stdlibDirectories = this->m_options.stdlibDirectories,
                                             uri = uri, text = text]() {
                                             parseAndSendDiagnostics(stdlibDirectories, uri, text);
                                         });
            } catch (nlohmann::json::exception &e) {
                std::cerr << "error at pos '" << pos << "':" << e.what() << "\n";
            }
        } else if (method == "textDocument/diagnostic") {
            auto params = json.at("params").get<nlohmann::json>();
            auto uri = params["textDocument"]["uri"].get<std::string>(); //.("uri");
            std::map<std::string, std::vector<parser::ParserMessasge> > errorsMap;
            std::stringstream text;
            if (m_openDocuments.contains(uri)) {
                text << m_openDocuments.at(uri).text;
                auto tokens = lexer::lex_file(uri, text.str());
                std::filesystem::path filePath = uri;
                std::unordered_map<std::string, bool> definitions;
                auto result = parser::parse_tokens(tokens);
                const auto nodes = modules::include_modules(m_options.stdlibDirectories, result);

                types::TypeCheckResult type_check_result;
                types::type_check(nodes, type_check_result);


                if (result.messages.empty() && type_check_result.messages.empty()) {
                    errorsMap[filePath.string()] = {};
                } else {
                    for (auto &error: result.messages) {
                        errorsMap[error.token.source_location.filename].push_back(error);
                    }
                    for (const auto &msg: type_check_result.messages) {
                        errorsMap[msg.token.source_location.filename].push_back(msg);
                    }
                }
                nlohmann::json diagnosticValues;
                nlohmann::json relatedDocuments;

                for (auto &[fileName, messsages]: errorsMap) {
                    nlohmann::json logMessages;
                    for (const auto &[outputType, token, message]: messsages) {
                        nlohmann::json logMessage;
                        nlohmann::json range;
                        range["start"] = buildPosition(token.source_location.row, token.source_location.col);
                        range["end"] = buildPosition(token.source_location.row,
                                                     token.source_location.col + token.source_location.num_bytes);
                        logMessage["range"] = std::move(range);
                        logMessage["severity"] = mapOutputTypeToSeverity(outputType);
                        logMessage["message"] = message;
                        nlohmann::json relatedInformations;
                        nlohmann::json source;
                        nlohmann::json location;
                        location["uri"] = token.source_location.filename;
                        nlohmann::json range2;
                        range2["start"] = buildPosition(token.source_location.row, token.source_location.col);
                        range2["end"] = buildPosition(token.source_location.row,
                                                      token.source_location.col + token.source_location.num_bytes);
                        location["range"] = std::move(range2);
                        source["location"] = std::move(location);
                        source["message"] = message;
                        source["source"] = "wirthx";
                        relatedInformations.push_back(std::move(source));

                        logMessage["relatedInformation"] = std::move(relatedInformations);
                        logMessages.push_back(std::move(logMessage));
                    }
                    if (fileName == uri) {
                        for (auto msg: logMessages)
                            diagnosticValues.push_back(std::move(msg));
                        logMessages.clear();
                    } else {
                        nlohmann::json diagnosticsReport;
                        diagnosticsReport["kind"] = "full";
                        diagnosticsReport["items"] = logMessages;

                        relatedDocuments[fileName] = diagnosticsReport;
                    }
                }
                nlohmann::json diagnosticsReport;
                diagnosticsReport["kind"] = "full";
                diagnosticsReport["items"] = diagnosticValues;
                diagnosticsReport["relatedDocuments"] = std::move(relatedDocuments);
                response["result"] = std::move(diagnosticsReport);
            } else {
                std::cerr << "Document not found for uri: " << uri << "\n";
                response["error"] = buildError("Document not found", -32603);
            }
        } else if (method == "textDocument/didClose") {
            auto params = json["params"];
            auto uri = params["textDocument"]["uri"].get<std::string>();
            m_openDocuments.erase(uri);
        } else if (method == "textDocument/didChange") {
            auto params = json["params"];
            auto uri = params["textDocument"]["uri"].get<std::string>();

            auto text = params["contentChanges"].front().at("text").get<std::string>();
            m_openDocuments[uri] =
                    LspDocument{.uri = uri, .text = text};
            response["result"] = nlohmann::json::object();
            std::ignore = std::async(std::launch::async, [rtlDirectories = this->m_options.stdlibDirectories,
                                         uri = uri, text = text]() {
                                         parseAndSendDiagnostics(rtlDirectories, uri, text);
                                     });
        }

        if (hasId) {
            std::stringstream sstream;

            sstream << response;
            std::cout << "Content-Length: " << sstream.str().length() << "\r\n\r\n";
            std::cout << sstream.str();
            std::cerr << "response: " << sstream.str() + "\n";
        }
    }
}
