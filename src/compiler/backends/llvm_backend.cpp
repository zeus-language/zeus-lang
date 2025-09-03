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
#include "llvm_intrinsics.h"
#include "ast/BinaryExpression.h"
#include "ast/FunctionCallNode.h"
#include "ast/NumberConstant.h"
#include "ast/ReturnStatement.h"
#include "ast/StringConstant.h"
#include "ast/VariableAccess.h"
#include "ast/VariableAssignment.h"
#include "ast/VariableDeclaration.h"

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

    llvm::GlobalVariable *getOrCreateGlobalString(LLVMBackendState &llvmState, const std::string &value,
                                                  const std::string &name) {
        auto _name = name;
        if (_name.empty()) {
            auto hash = std::hash<std::string>{}(value);
            _name = "string." + std::to_string(hash);
        }

        if (const auto var = llvmState.TheModule->getGlobalVariable(_name, true))
            return var;
        return llvmState.Builder->CreateGlobalString(value, _name);
    }


    llvm::Value *codegen(ast::ReturnStatement *node, LLVMBackendState &llvmState);

    llvm::Value *codegen(ast::FunctionCallNode *node, LLVMBackendState &llvmState);

    llvm::Value *codegen(const ast::NumberConstant *node, const LLVMBackendState &llvmState);

    llvm::Value *codegen(ast::StringConstant *node, LLVMBackendState &llvmState);

    llvm::Value *codegen(ast::VariableDeclaration *node, LLVMBackendState &llvmState);

    llvm::Value *codegen(ast::VariableAssignment *node, LLVMBackendState &llvmState);

    llvm::Value *codegen(ast::VariableAccess *node, LLVMBackendState &llvmState);

    llvm::Value *codegen(ast::BinaryExpression *node, LLVMBackendState &llvmState);

    llvm::Value *codegen_base(ast::ASTNode *node, LLVMBackendState &llvmState) {
        if (const auto returnStatement = dynamic_cast<ast::ReturnStatement *>(node)) {
            return llvm_backend::codegen(returnStatement, llvmState);
        }
        if (const auto functionCall = dynamic_cast<ast::FunctionCallNode *>(node)) {
            return llvm_backend::codegen(functionCall, llvmState);
        }
        if (const auto number = dynamic_cast<ast::NumberConstant *>(node)) {
            return llvm_backend::codegen(number, llvmState);
        }
        if (const auto string = dynamic_cast<ast::StringConstant *>(node)) {
            return llvm_backend::codegen(string, llvmState);
        }
        if (const auto varDecl = dynamic_cast<ast::VariableDeclaration *>(node)) {
            return llvm_backend::codegen(varDecl, llvmState);
        }
        if (const auto assignment = dynamic_cast<ast::VariableAssignment *>(node)) {
            return llvm_backend::codegen(assignment, llvmState);
        }
        if (const auto varAccess = dynamic_cast<ast::VariableAccess *>(node)) {
            return llvm_backend::codegen(varAccess, llvmState);
        }
        if (const auto binExpr = dynamic_cast<ast::BinaryExpression *>(node)) {
            return llvm_backend::codegen(binExpr, llvmState);
        }

        // Handle other node types or throw an error
        assert(false && "Unknown AST node type for code generation");
        return nullptr; // Placeholder
    }

    llvm::Value *codegen(ast::BinaryExpression *node, LLVMBackendState &llvmState) {
        auto lhs = codegen_base(node->lhs().get(), llvmState);
        auto rhs = codegen_base(node->rhs().get(), llvmState);

        switch (node->binoperator()) {
            case ast::BinaryOperator::ADD:
                if (lhs->getType()->isIntegerTy() && rhs->getType()->isIntegerTy()) {
                    return llvmState.Builder->CreateAdd(lhs, rhs, "addtmp");
                } else if (lhs->getType()->isFloatingPointTy() && rhs->getType()->isFloatingPointTy()) {
                    return llvmState.Builder->CreateFAdd(lhs, rhs, "faddtmp");
                }
                break;
            case ast::BinaryOperator::SUB:
                if (lhs->getType()->isIntegerTy() && rhs->getType()->isIntegerTy()) {
                    return llvmState.Builder->CreateSub(lhs, rhs, "subtmp");
                } else if (lhs->getType()->isFloatingPointTy() && rhs->getType()->isFloatingPointTy()) {
                    return llvmState.Builder->CreateFSub(lhs, rhs, "fsubtmp");
                }
                break;
            case ast::BinaryOperator::MUL:
                if (lhs->getType()->isIntegerTy() && rhs->getType()->isIntegerTy()) {
                    return llvmState.Builder->CreateMul(lhs, rhs, "multmp");
                } else if (lhs->getType()->isFloatingPointTy() && rhs->getType()->isFloatingPointTy()) {
                    return llvmState.Builder->CreateFMul(lhs, rhs, "fmultmp");
                }
                break;
            case ast::BinaryOperator::DIV:
                if (lhs->getType()->isIntegerTy() && rhs->getType()->isIntegerTy()) {
                    return llvmState.Builder->CreateSDiv(lhs, rhs, "divtmp");
                } else if (lhs->getType()->isFloatingPointTy() && rhs->getType()->isFloatingPointTy()) {
                    return llvmState.Builder->CreateFDiv(lhs, rhs, "fdivtmp");
                }
                break;
            default:
                assert(false && "Unsupported binary operator");
        }
        assert(false && "Type mismatch in binary expression");
    }


    llvm::Value *codegen(ast::VariableAccess *node, LLVMBackendState &llvmState) {
        ;
        if (const auto alloca = llvmState.NamedAllocations[node->expressionToken().lexical()]) {
            return llvmState.Builder->CreateLoad(alloca->getAllocatedType(), alloca, node->expressionToken().lexical());
        }
        if (const auto value = llvmState.NamedValues[node->expressionToken().lexical()]) {
            return value;
        }
        assert(false && "Variable not declared before access");
        return nullptr;
    }

    llvm::Value *codegen(ast::VariableAssignment *node, LLVMBackendState &llvmState) {
        const auto alloca = llvmState.NamedAllocations[node->expressionToken().lexical()];
        if (!alloca) {
            assert(false && "Variable not declared before assignment");
            return nullptr;
        }
        llvmState.Builder->CreateStore(codegen_base(node->expression().get(), llvmState), alloca);


        return alloca; // Return the allocation instruction
    }

    llvm::Value *codegen(ast::VariableDeclaration *node, LLVMBackendState &llvmState) {
        llvm::Type *varType = nullptr;
        if (node->type().lexical() == "i32") {
            varType = llvmState.Builder->getInt32Ty();
        } else if (node->type().lexical() == "f64") {
            varType = llvmState.Builder->getDoubleTy();
        } else if (node->type().lexical() == "string") {
            varType = llvmState.Builder->getPtrTy();
        } else {
            assert(false && "Unsupported variable type");
            return nullptr;
        }

        if (node->constant()) {
            llvmState.NamedValues[node->expressionToken().lexical()] = nullptr;
            // Handle constant variable declaration if needed
            if (const auto initialValue = node->initialValue()) {
                if (llvm::Value *initValue = codegen_base(initialValue.value().get(), llvmState)) {
                    llvmState.NamedValues[node->expressionToken().lexical()] = initValue;
                } else {
                    assert(false && "Failed to generate initial value for variable");
                    return nullptr;
                }
            }
            return llvmState.NamedValues[node->expressionToken().lexical()];
        }


        llvm::AllocaInst *alloca = llvmState.Builder->CreateAlloca(varType, nullptr, node->expressionToken().lexical());
        llvmState.NamedAllocations[node->expressionToken().lexical()] = alloca;

        if (auto initialValue = node->initialValue()) {
            if (llvm::Value *initValue = codegen_base(initialValue.value().get(), llvmState)) {
                llvmState.Builder->CreateStore(initValue, alloca);
            } else {
                assert(false && "Failed to generate initial value for variable");
                return nullptr;
            }
        } else {
            // Default initialization
            if (varType->isIntegerTy()) {
                llvm::Value *defaultValue = llvm::ConstantInt::get(varType, 0);
                llvmState.Builder->CreateStore(defaultValue, alloca);
            } else if (varType->isFloatingPointTy()) {
                llvm::Value *defaultValue = llvm::ConstantFP::get(varType, 0.0);
                llvmState.Builder->CreateStore(defaultValue, alloca);
            } else if (varType->isPointerTy()) {
                llvm::Value *defaultValue = llvm::ConstantPointerNull::get(
                    llvm::cast<llvm::PointerType>(varType));
                llvmState.Builder->CreateStore(defaultValue, alloca);
            } else {
                assert(false && "Unsupported variable type for default initialization");
                return nullptr;
            }
        }

        return alloca; // Return the allocation instruction
    }

    llvm::Value *codegen(const ast::NumberConstant *node, const LLVMBackendState &llvmState) {
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
        return getOrCreateGlobalString(llvmState, node->expressionToken().lexical(), "");
    }

    llvm::Value *codegen(ast::FunctionCallNode *node, LLVMBackendState &llvmState) {
        if (node->functionName() == "println") {
            const auto printfFunc = llvmState.TheModule->getFunction("printf");
            if (!printfFunc) {
                return nullptr; // Error handling
            }
            std::vector<llvm::Value *> args;

            assert(node->args().size() == 1 && "println expects exactly one argument");
            for (const auto &arg: node->args()) {
                auto value = codegen_base(arg.get(), llvmState);
                if (value->getType()->isIntegerTy(32)) {
                    args.push_back(getOrCreateGlobalString(llvmState, "%d\n", "i32_format"));
                } else if (value->getType()->isIntegerTy(64)) {
                    args.push_back(getOrCreateGlobalString(llvmState, "%ld\n", "i64_format"));
                } else if (value->getType()->isDoubleTy()) {
                    args.push_back(getOrCreateGlobalString(llvmState, "%f\n", "double_format"));
                } else if (value->getType()->isFloatTy()) {
                    args.push_back(getOrCreateGlobalString(llvmState, "%f\n", "float_format"));
                } else if (value->getType()->isPointerTy()) {
                    args.push_back(getOrCreateGlobalString(llvmState, "%s\n", "string_format"));
                } else {
                    assert(false && "Unsupported argument type for println");
                    return nullptr;
                }

                args.push_back(value);
            }
            return llvmState.Builder->CreateCall(printfFunc, args, "printfCall");
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

    void init_context(LLVMBackendState &context, const std::string &moduleName,
                      const compiler::CompilerOptions &options) {
        context.TheContext = std::make_unique<llvm::LLVMContext>();
        context.TheModule = std::make_unique<llvm::Module>(moduleName, *context.TheContext);
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
        if (options.buildMode == compiler::BuildMode::Release) {
            // Combine redundant instructions.
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
        }
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
                                      const std::vector<std::unique_ptr<ast::ASTNode> > &nodes) {
    initializeLLVMBackend();
    LLVMBackendState context;
    init_context(context, moduleName, options);
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

    createPrintfCall(*context.TheContext, *context.TheModule);

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
