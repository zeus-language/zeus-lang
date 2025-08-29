#include "llvm_backend.h"


#include "../CompilerOptions.h"
#include "ast/ASTNode.h"
#include "ast/FunctionDefinition.h"
#include "llvm/IR/PassManager.h"

#include "llvm/IR/LegacyPassManager.h"

#include "llvm/IR/Verifier.h"
#include "llvm/LTO/LTO.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Passes/StandardInstrumentations.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/TargetParser/Host.h"
#include "llvm/Transforms/IPO/AlwaysInliner.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include <llvm/Analysis/CGSCCPassManager.h>
#include <llvm/Analysis/LoopAnalysisManager.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Passes/StandardInstrumentations.h>
#include <llvm/Transforms/IPO/AlwaysInliner.h>
#include <llvm/Transforms/IPO/GlobalDCE.h>
#include <llvm/Transforms/IPO/PartialInlining.h>
#include <llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm/Transforms/Scalar/DCE.h>
#include <llvm/Transforms/Scalar/GVN.h>
#include <llvm/Transforms/Scalar/MemCpyOptimizer.h>
#include <llvm/Transforms/Scalar/Reassociate.h>
#include <llvm/Transforms/Scalar/SCCP.h>
#include <llvm/Transforms/Scalar/SimplifyCFG.h>
#include <llvm/Transforms/Utils/LoopSimplify.h>
#include "command.h"
#include "linker.h"
#include "ast/FunctionCallNode.h"
#include "ast/NumberConstant.h"
#include "ast/ReturnStatement.h"
#include "ast/StringConstant.h"

namespace llvm_backend {
    struct LLVMBackendState {
        std::unique_ptr<llvm::LLVMContext> TheContext;
        std::unique_ptr<llvm::Module> TheModule;
        std::unique_ptr<llvm::IRBuilder<llvm::ConstantFolder, llvm::IRBuilderDefaultInserter> > Builder;
        std::unordered_map<std::string, llvm::AllocaInst *> NamedAllocations;
        std::unordered_map<std::string, llvm::Value *> NamedValues;
        llvm::Function *TopLevelFunction{};
        std::unordered_map<std::string, llvm::Function *> FunctionDefinitions;

        std::unique_ptr<llvm::FunctionPassManager> TheFPM;
        std::unique_ptr<llvm::FunctionAnalysisManager> TheFAM;
        std::unique_ptr<llvm::ModuleAnalysisManager> TheMAM;
        std::unique_ptr<llvm::ModulePassManager> TheMPM;
        std::unique_ptr<llvm::PassInstrumentationCallbacks> ThePIC;
        std::unique_ptr<llvm::StandardInstrumentations> TheSI;
    };

    void initializeLLVMBackend() {
        using namespace llvm;
        using namespace llvm::sys;
        // Initialize the target registry etc.InitializeAllTargetInfos();
        // InitializeAllTargets();
        // InitializeAllTargetMCs();
        // InitializeAllAsmParsers();
        // InitializeAllAsmPrinters();

        InitializeNativeTarget();
        InitializeNativeTargetAsmParser();
        InitializeNativeTargetAsmPrinter();
    }

    llvm::Value *codegen(ast::ReturnStatement *node, LLVMBackendState &llvmState);

    llvm::Value *codegen(ast::FunctionCallNode *node, LLVMBackendState &llvmState);

    llvm::Value *codegen(ast::NumberConstant *node, LLVMBackendState &llvmState);

    llvm::Value *codegen(ast::StringConstant *node, LLVMBackendState &llvmState);

    llvm::Value *codegen_base(ast::ASTNode *node, LLVMBackendState &llvmState) {
        if (auto returnStatement = dynamic_cast<ast::ReturnStatement *>(node)) {
            return llvm_backend::codegen(returnStatement, llvmState);
        }
        if (auto functionCall = dynamic_cast<ast::FunctionCallNode *>(node)) {
            return llvm_backend::codegen(functionCall, llvmState);
        }
        if (auto number = dynamic_cast<ast::NumberConstant *>(node)) {
            return llvm_backend::codegen(number, llvmState);
        }
        if (auto string = dynamic_cast<ast::StringConstant *>(node)) {
            return llvm_backend::codegen(string, llvmState);
        }

        // Handle other node types or throw an error
        assert(false && "Unknown AST node type for code generation");
        return nullptr; // Placeholder
    }

    llvm::Value *codegen(ast::NumberConstant *node, LLVMBackendState &llvmState) {
        switch (node->numberType()) {
            case ast::NumberType::INTEGER:
                return llvm::ConstantInt::get(*llvmState.TheContext,
                                              llvm::APInt(32, std::get<int64_t>(node->value()), true));
            case ast::NumberType::FLOAT:
                return llvm::ConstantFP::get(*llvmState.TheContext, llvm::APFloat(std::get<double>(node->value())));
        }
        assert(false && "unknown number type in codegen");
        return nullptr;
    }

    llvm::Value *codegen(ast::StringConstant *node, LLVMBackendState &llvmState) {
        return nullptr;
    }

    llvm::Value *codegen(ast::FunctionCallNode *node, LLVMBackendState &llvmState) {
        if (node->functionName() == "println") {
            // Example: calling printf for println
            auto printfFunc = llvmState.TheModule->getFunction("printf");
            if (!printfFunc) {
                return nullptr; // Error handling
            }
            std::vector<llvm::Value *> args;
            for (const auto &arg: node->args()) {
                args.push_back(codegen_base(arg.get(), llvmState));
            }
        }
        return nullptr; // Placeholder
    }

    llvm::Value *codegen(ast::ReturnStatement *node, LLVMBackendState &llvmState) {
        if (auto returnValue = node->returnValue()) {
            // Generate code for the return expression
            // Placeholder for actual expression code generation
            llvm::Value *retValue = llvm_backend::codegen_base(returnValue.value(), llvmState);
            // Example: returning 0
            return llvmState.Builder->CreateRet(retValue);
        } else {
            return llvmState.Builder->CreateRetVoid();
        }
    }

    void codegen(ast::FunctionDefinition *node, LLVMBackendState &llvmState) {
        // Placeholder for actual LLVM IR generation logic
        auto resultType = llvmState.Builder->getInt32Ty();
        std::vector<llvm::Type *> params;
        for (const auto &param: node->args()) {
            //TODO
        }
        llvm::FunctionType *FT = llvm::FunctionType::get(resultType, params, false);
        auto linkage = llvm::Function::ExternalLinkage;


        auto functionDefinition = llvm::Function::Create(FT, linkage, node->functionName(),
                                                         llvmState.TheModule.get());

        llvm::BasicBlock *functionBaseBlock = llvm::BasicBlock::Create(*llvmState.TheContext,
                                                                       functionDefinition->getName(),
                                                                       functionDefinition);

        llvmState.Builder->SetInsertPoint(functionBaseBlock);
        for (auto &stmt: node->statements()) {
            llvm_backend::codegen_base(stmt.get(), llvmState);
        }

        // if (!m_returnType)
        // {
        //     context->builder()->CreateRetVoid();
        // }

        if (llvm::verifyFunction(*functionDefinition, &llvm::errs())) {
            llvmState.TheFPM->run(*functionDefinition, *llvmState.TheFAM);
        }
    }

    void init_context(LLVMBackendState &context) {
        context.TheContext = std::make_unique<llvm::LLVMContext>();
        context.TheModule = std::make_unique<llvm::Module>("my_module", *context.TheContext);
        context.Builder = std::make_unique<llvm::IRBuilder<> >(*context.TheContext);
        context.TheFPM = std::make_unique<llvm::FunctionPassManager>();
        context.TheMPM = std::make_unique<llvm::ModulePassManager>();
        context.TheFAM = std::make_unique<llvm::FunctionAnalysisManager>();
        context.TheMAM = std::make_unique<llvm::ModuleAnalysisManager>();

        context.ThePIC = std::make_unique<llvm::PassInstrumentationCallbacks>();
        context.TheSI = std::make_unique<llvm::StandardInstrumentations>(*context.TheContext,
                                                                         /*DebugLogging*/ true);

        context.TheSI->registerCallbacks(*context.ThePIC, context.TheMAM.get());

        // Add transform passes.

        context.TheFPM->addPass(llvm::InstCombinePass());
        // Reassociate expressions.
        context.TheFPM->addPass(llvm::ReassociatePass());
        // Eliminate Common SubExpressions.
        context.TheFPM->addPass(llvm::GVNPass());
        // Simplify the control flow graph (deleting unreachable blocks, etc).
        context.TheFPM->addPass(llvm::SimplifyCFGPass());

        context.TheFPM->addPass(llvm::SCCPPass());

        context.TheFPM->addPass(llvm::LoopSimplifyPass());

        context.TheFPM->addPass(llvm::MemCpyOptPass());

        context.TheFPM->addPass(llvm::DCEPass());
        context.TheMPM->addPass(llvm::AlwaysInlinerPass());


        context.TheMPM->addPass(llvm::PartialInlinerPass());
        context.TheMPM->addPass(llvm::ModuleInlinerPass());
        context.TheMPM->addPass(llvm::GlobalDCEPass());

        // how do i remove unused functions?


        context.TheMPM->addPass(llvm::createModuleToFunctionPassAdaptor(
            llvm::DCEPass())); // Remove dead functions and global variables.
    }

    // Register analysis passes used in these transform passes.
    llvm::PassBuilder PB;
    const auto TheLAM = std::make_unique<llvm::LoopAnalysisManager>();
    const auto TheCGAM = std::make_unique<llvm::CGSCCAnalysisManager>();
    const auto TheFAM = std::make_unique<llvm::FunctionAnalysisManager>();
    const auto TheMAM = std::make_unique<llvm::ModuleAnalysisManager>();
}

void llvm_backend::generateExecutable(const compiler::CompilerOptions &options, const std::string &moduleName,
                                      std::ostream &errorStream,
                                      std::ostream &outputStream,
                                      const std::string &outputPath,
                                      const std::vector<std::unique_ptr<ast::ASTNode> > &nodes) {
    initializeLLVMBackend();
    LLVMBackendState context;
    init_context(context);
    auto TargetTriple = llvm::sys::getDefaultTargetTriple();
    std::string Error;

    auto Target = llvm::TargetRegistry::lookupTarget(TargetTriple, Error);
    if (!Target) {
        llvm::errs() << Error << "\n" << "Tiplet: " << TargetTriple << "\n";
        return;
    }
    auto CPU = "generic";
    auto Features = "";

    llvm::TargetOptions opt;
    auto TheTargetMachine = Target->createTargetMachine(TargetTriple, CPU, Features, opt, llvm::Reloc::PIC_);
    llvm::Triple target(TargetTriple);


    for (auto &node: nodes) {
        if (auto funcDef = dynamic_cast<ast::FunctionDefinition *>(node.get())) {
            llvm_backend::codegen(funcDef, context);
        }
    }


    auto basePath = options.outputDirectory;


    auto objectFileName = basePath / (moduleName + ".o");
    std::vector<std::string> objectFiles;
    objectFiles.emplace_back(objectFileName.string());
    std::error_code EC;
    llvm::raw_fd_ostream dest(objectFileName.string(), EC, llvm::sys::fs::OF_None);

    if (EC) {
        llvm::errs() << "Could not open file: " << EC.message();
        return;
    }

    llvm::legacy::PassManager pass;
    if (options.buildMode == compiler::BuildMode::Release) {
        pass.add(llvm::createAlwaysInlinerLegacyPass());
        pass.add(llvm::createInstructionCombiningPass());
        TheTargetMachine->setOptLevel(llvm::CodeGenOptLevel::Aggressive);
    } else {
        TheTargetMachine->setOptLevel(llvm::CodeGenOptLevel::None);
    }


    if (TheTargetMachine->addPassesToEmitFile(pass, dest, nullptr, llvm::CodeGenFileType::ObjectFile)) {
        llvm::errs() << "TheTargetMachine can't emit a file of this type";
        return;
    }

    pass.run(*context.TheModule);
    dest.flush();
    dest.close();


    llvm::verifyModule(*context.TheModule, &llvm::errs());
    if (options.printLLVMIR) {
        context.TheModule->print(llvm::errs(), nullptr, false, false);
    }

    llvm::outs() << "Wrote " << objectFileName.string() << "\n";

    std::vector<std::string> flags;
    // for (const auto &lib: context->programUnit()->collectLibsToLink())
    // {
    //     flags.push_back("-l" + lib);
    // }


    if (options.buildMode == compiler::BuildMode::Debug && target.getOS() != llvm::Triple::Win32) {
        flags.emplace_back("-fsanitize=address");
        flags.emplace_back("-fno-omit-frame-pointer");
    }

    std::string executableName = moduleName;

    if (target.getOS() == llvm::Triple::Win32) {
        executableName += ".exe";
        flags.erase(std::ranges::find(flags, "-lc"));
    }

    if (!link_modules(errorStream, basePath, executableName, flags, objectFiles)) {
        return;
    }

    if (options.runProgram) {
        if (!execute_command(outputStream, errorStream, (basePath / executableName).string())) {
            errorStream << "program could not be executed!\n";
        }
    }
}
