//
// Created by stefan on 29.09.25.
//

#ifndef ZEUS_PARENT_INITIALIZEPARAMS_H
#define ZEUS_PARENT_INITIALIZEPARAMS_H
#include <string>

namespace lsp {
    struct LspAny {
    };

    struct ClientCapabilities {
    };

    struct InitializeParams {
        int processId;
        std::string rootPath;
        std::string rootUri;
        LspAny initializationOptions;
        ClientCapabilities capabilities;
    };
} // lsp

#endif //ZEUS_PARENT_INITIALIZEPARAMS_H
