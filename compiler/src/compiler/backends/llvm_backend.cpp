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
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Transforms/IPO/GlobalDCE.h>
#include <llvm/Transforms/IPO/PartialInlining.h>
#include <llvm/Transforms/Scalar/DCE.h>
#include <llvm/Transforms/Scalar/GVN.h>
#include <llvm/Transforms/Scalar/MemCpyOptimizer.h>
#include <llvm/Transforms/Scalar/Reassociate.h>
#include <llvm/Transforms/Scalar/SCCP.h>
#include <llvm/Transforms/Scalar/SimplifyCFG.h>
#include <llvm/Transforms/Utils/LoopSimplify.h>
#include "os/command.h"
#include "linker/linker.h"
#include "llvm_intrinsics.h"
#include "ast/ArrayAccess.h"
#include "ast/ArrayInitializer.h"
#include "ast/BinaryExpression.h"
#include "ast/BreakStatement.h"
#include "ast/Comparisson.h"
#include "ast/ForLoop.h"
#include "ast/FunctionCallNode.h"
#include "ast/IfCondition.h"
#include "ast/LogicalExpression.h"
#include "ast/NumberConstant.h"
#include "ast/ReturnStatement.h"
#include "ast/StringConstant.h"
#include "ast/VariableAccess.h"
#include "ast/VariableAssignment.h"
#include "ast/VariableDeclaration.h"
#include "ast/WhileLoop.h"

namespace llvm_backend {
    struct BreakBlock {
        llvm::BasicBlock *afterLoop = nullptr;
        llvm::BasicBlock *currentLoop = nullptr;
    };

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
        BreakBlock currentBreakBlock;

        llvm::Value *findVariable(const std::string &name) {
            if (NamedValues.contains(name)) {
                return NamedValues[name];
            }
            if (NamedAllocations.contains(name)) {
                return Builder->CreateLoad(NamedAllocations[name]->getAllocatedType(), NamedAllocations[name], name);
            }
            return nullptr;
        }
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

    llvm::Type *resolveLlvmType(std::shared_ptr<types::VariableType> value, LLVMBackendState &context) {
        if (auto intType = std::dynamic_pointer_cast<types::IntegerType>(value)) {
            return llvm::Type::getIntNTy(*context.TheContext, intType->size() * 8);
        }

        switch (value->typeKind()) {
            case types::TypeKind::INT:
                break;
            case types::TypeKind::FLOAT:
                return llvm::Type::getFloatTy(*context.TheContext);
            case types::TypeKind::STRING:
                return llvm::PointerType::getUnqual(*context.TheContext);
            case types::TypeKind::BOOL:
                return llvm::Type::getInt1Ty(*context.TheContext);
            case types::TypeKind::VOID:
                return llvm::Type::getVoidTy(*context.TheContext);
            case types::TypeKind::STRUCT:
                break;
            case types::TypeKind::ARRAY:
                if (auto arrayType = std::dynamic_pointer_cast<types::ArrayType>(value)) {
                    auto baseType = resolveLlvmType(arrayType->baseType(), context);
                    if (baseType == nullptr) return nullptr;
                    return llvm::ArrayType::get(baseType, arrayType->size());
                }
                break;
            case types::TypeKind::POINTER:
                return llvm::PointerType::getUnqual(*context.TheContext);
        }
        assert(false && "Unknown type");
        return nullptr;
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

    llvm::Value *codegen(const ast::VariableDeclaration *node, LLVMBackendState &llvmState);

    llvm::Value *codegen(ast::VariableAssignment *node, LLVMBackendState &llvmState);

    llvm::Value *codegen(ast::VariableAccess *node, LLVMBackendState &llvmState);

    llvm::Value *codegen(ast::BinaryExpression *node, LLVMBackendState &llvmState);

    llvm::Value *codegen(ast::IfCondition *node, LLVMBackendState &llvmState);

    llvm::Value *codegen(ast::WhileLoop *node, LLVMBackendState &llvmState);

    llvm::Value *codegen(ast::LogicalExpression *node, LLVMBackendState &llvmState);

    llvm::Value *codegen(ast::Comparisson *node, LLVMBackendState &llvmState);

    llvm::Value *codegen(ast::ForLoop *node, LLVMBackendState &llvmState);

    llvm::Value *codegen(ast::BreakStatement *node, LLVMBackendState &llvmState);

    llvm::Value *codegen(ast::ArrayInitializer *node, LLVMBackendState &llvmState);

    llvm::Value *codegen(ast::ArrayAccess *node, LLVMBackendState &llvmState);

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
        if (const auto ifCond = dynamic_cast<ast::IfCondition *>(node)) {
            return llvm_backend::codegen(ifCond, llvmState);
        }
        if (const auto logExpr = dynamic_cast<ast::LogicalExpression *>(node)) {
            return llvm_backend::codegen(logExpr, llvmState);
        }
        if (const auto comp = dynamic_cast<ast::Comparisson *>(node)) {
            return llvm_backend::codegen(comp, llvmState);
        }
        if (const auto whileLoop = dynamic_cast<ast::WhileLoop *>(node)) {
            return llvm_backend::codegen(whileLoop, llvmState);
        }
        if (const auto forLoop = dynamic_cast<ast::ForLoop *>(node)) {
            return llvm_backend::codegen(forLoop, llvmState);
        }
        if (const auto breakStmt = dynamic_cast<ast::BreakStatement *>(node)) {
            return llvm_backend::codegen(breakStmt, llvmState);
        }
        if (const auto arrayInit = dynamic_cast<ast::ArrayInitializer *>(node)) {
            return llvm_backend::codegen(arrayInit, llvmState);
        }
        if (const auto arrayAccess = dynamic_cast<ast::ArrayAccess *>(node)) {
            return llvm_backend::codegen(arrayAccess, llvmState);
        }

        // Handle other node types or throw an error
        assert(false && "Unknown AST node type for code generation");
        return nullptr; // Placeholder
    }

    llvm::Value *codegen(ast::ArrayInitializer *node, LLVMBackendState &llvmState) {
        if (node->elements().empty()) {
            return llvm::Constant::getNullValue(llvm::ArrayType::get(
                llvm::Type::getInt8Ty(*llvmState.TheContext), 0));
        } else {
            std::vector<llvm::Constant *> elements;
            for (auto &element: node->elements()) {
                auto elemValue = codegen_base(element.get(), llvmState);
                if (!elemValue) {
                    assert(false && "Failed to generate element value for array initializer");
                    return nullptr;
                }
                if (auto constElem = llvm::dyn_cast<llvm::Constant>(elemValue)) {
                    elements.push_back(constElem);
                } else {
                    assert(false && "Array initializer element is not a constant");
                    return nullptr;
                }
            }
            auto firstElemType = elements[0]->getType();
            for (const auto &elem: elements) {
                if (elem->getType() != firstElemType) {
                    assert(false && "Array initializer elements have different types");
                    return nullptr;
                }
            }
            auto arrayType = llvm::ArrayType::get(firstElemType, elements.size());
            auto arrayConstant = llvm::ConstantArray::get(arrayType, elements);
            auto hash = std::hash<std::string>{}(node->expressionToken().lexical());
            std::string arrayName = "array." + std::to_string(hash);
            auto arrayVar = new llvm::GlobalVariable(
                *llvmState.TheModule,
                arrayType,
                true,
                llvm::GlobalValue::PrivateLinkage,
                arrayConstant,
                arrayName
            );
            return arrayVar;
        }
    }

    llvm::Value *codegen(ast::ArrayAccess *node, LLVMBackendState &llvmState) {
        auto arrayPtr = llvmState.findVariable(node->expressionToken().lexical());
        auto arrayType = resolveLlvmType(node->arrayType().value(), llvmState);
        auto indexValue = codegen_base(node->index(), llvmState);
        if (!indexValue) {
            assert(false && "Failed to generate index value for array access");
            return nullptr;
        }
        if (!indexValue->getType()->isIntegerTy()) {
            assert(false && "Array index is not an integer");
            return nullptr;
        }
        std::vector<llvm::Value *> indices;
        auto zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*llvmState.TheContext), 0);
        indices.push_back(zero);
        if (indexValue->getType() != llvm::Type::getInt32Ty(*llvmState.TheContext)) {
            indexValue = llvmState.Builder->CreateIntCast(indexValue, llvm::Type::getInt32Ty(*llvmState.TheContext),
                                                          true, "array_index_cast");
        }
        indices.push_back(indexValue);


        auto elementPtr = llvmState.Builder->CreateGEP(arrayType, arrayPtr, indices, "elem_ptr");
        auto loadedValue = llvmState.Builder->CreateLoad(arrayType->getArrayElementType(),
                                                         elementPtr,
                                                         "array_elem");
        return loadedValue;
    }

    llvm::Value *codegen(ast::BreakStatement *node, LLVMBackendState &llvmState) {
        return llvmState.Builder->CreateBr(llvmState.currentBreakBlock.afterLoop);
    }

    llvm::Value *codegen_iterator_for(ast::ForLoop *node, LLVMBackendState &llvmState) {
        llvm::Function *TheFunction = llvmState.Builder->GetInsertBlock()->getParent();

        llvm::BasicBlock *PreheaderBB = llvmState.Builder->GetInsertBlock();
        llvm::BasicBlock *LoopBB = llvm::BasicBlock::Create(*llvmState.TheContext, "loop", TheFunction);
        llvm::BasicBlock *AfterBB = llvm::BasicBlock::Create(*llvmState.TheContext, "afterloop", TheFunction);

        // Insert an explicit fall through from the current block to the LoopBB.
        llvmState.Builder->CreateBr(LoopBB);

        // Start insertion in LoopBB.
        llvmState.Builder->SetInsertPoint(LoopBB);

        // Create the PHI node to hold the loop variable.
        const auto varType = node->expressionType();
        if (!varType) {
            assert(false && "Could not determine type of iterator variable in for loop");
            return nullptr;
        }
        auto llvmVarType = resolveLlvmType(varType.value(), llvmState);
        if (!llvmVarType) {
            assert(false && "Could not resolve LLVM type of iterator variable in for loop");
            return nullptr;
        }

        llvm::PHINode *Variable = llvmState.Builder->CreatePHI(llvmVarType, 2);


        auto iterableValue = codegen_base(node->rangeStart(), llvmState);
        auto iterableType = node->rangeStart()->expressionType();
        if (!iterableValue) {
            assert(false && "Failed to generate iterable value for for loop");
            return nullptr;
        }
        size_t arraySize = 0;
        if (auto arrayType = std::dynamic_pointer_cast<types::ArrayType>(iterableType.value())) {
            arraySize = arrayType->size();
        } else {
            assert(false && "Iterable in for loop is not an array");
            return nullptr;
        }

        llvm::Value *startValue = llvmState.Builder->getInt32(0);

        // load array value
        auto arrayType = resolveLlvmType(node->rangeStart()->expressionType().value(), llvmState);
        auto arrayAllocation = llvmState.findVariable(node->rangeStart()->expressionToken().lexical());

        if (startValue->getType() != llvmVarType) {
            startValue = llvmState.Builder->CreateIntCast(startValue, llvmVarType, true, "for_start_cast");
        } {
            std::vector<llvm::Value *> indices;
            auto zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*llvmState.TheContext), 0);
            indices.push_back(zero);
            indices.push_back(Variable);
            auto elementPtr = llvmState.Builder->CreateGEP(arrayType, arrayAllocation, indices,
                                                           "elem_ptr");
            llvm::Value *loadedValue = llvmState.Builder->CreateLoad(
                arrayType->getArrayElementType(),
                elementPtr,
                "array_elem");
            if (loadedValue->getType() != llvmVarType) {
                loadedValue = llvmState.Builder->CreateIntCast(loadedValue, llvmVarType, true, "for_elem_cast");
            }
            if (node->isConstant()) {
                llvmState.NamedValues[node->iteratorToken().lexical()] = loadedValue;
            }
        }


        llvmState.currentBreakBlock.currentLoop = LoopBB;
        llvmState.currentBreakBlock.afterLoop = AfterBB;
        // Generate the loop body.
        for (auto &exp: node->block()) {
            llvmState.Builder->SetInsertPoint(LoopBB);

            codegen_base(exp.get(), llvmState);
        }

        // Step: increment the loop variable.
        const auto stepValue = llvm::ConstantInt::get(llvmVarType, 1);
        const auto nextVar = llvmState.Builder->CreateAdd(Variable, stepValue, "nextvar");

        Variable->addIncoming(startValue, PreheaderBB);

        // Compute the end condition.
        llvm::Value *endValue = llvmState.Builder->getInt32(arraySize);
        if (!endValue) {
            assert(false && "Failed to generate end value for for loop");
            return nullptr;
        }
        if (endValue->getType() != llvmVarType) {
            endValue = llvmState.Builder->CreateIntCast(endValue, llvmVarType, true, "for_end_cast");
        }
        llvm::Value *endCond = llvmState.Builder->CreateICmpSLT(nextVar, endValue, "loopcond");

        llvm::BasicBlock *loopEndBB = llvmState.Builder->GetInsertBlock();

        // Create the "after loop" block and insert it.
        llvmState.Builder->CreateCondBr(endCond, LoopBB, AfterBB);
        llvmState.Builder->SetInsertPoint(AfterBB);

        // Add the incoming value for the PHI node from the backedge.
        Variable->addIncoming(nextVar, loopEndBB);
        return llvm::Constant::getNullValue(llvm::Type::getInt32Ty(*llvmState.TheContext));
    }

    llvm::Value *codegen_range_for(ast::ForLoop *node, LLVMBackendState &llvmState) {
        llvm::Function *TheFunction = llvmState.Builder->GetInsertBlock()->getParent();

        llvm::BasicBlock *PreheaderBB = llvmState.Builder->GetInsertBlock();
        llvm::BasicBlock *LoopBB = llvm::BasicBlock::Create(*llvmState.TheContext, "loop", TheFunction);
        llvm::BasicBlock *AfterBB = llvm::BasicBlock::Create(*llvmState.TheContext, "afterloop", TheFunction);

        // Insert an explicit fall through from the current block to the LoopBB.
        llvmState.Builder->CreateBr(LoopBB);

        // Start insertion in LoopBB.
        llvmState.Builder->SetInsertPoint(LoopBB);

        // Create the PHI node to hold the loop variable.
        const auto varType = node->expressionType();
        if (!varType) {
            assert(false && "Could not determine type of iterator variable in for loop");
            return nullptr;
        }
        auto llvmVarType = resolveLlvmType(varType.value(), llvmState);
        if (!llvmVarType) {
            assert(false && "Could not resolve LLVM type of iterator variable in for loop");
            return nullptr;
        }

        llvm::PHINode *Variable = llvmState.Builder->CreatePHI(llvmVarType, 2, node->iteratorToken().lexical());
        llvmState.NamedValues[node->iteratorToken().lexical()] = Variable;

        // Initialize the PHI node with the start value.
        auto startValue = codegen_base(node->rangeStart(), llvmState);
        if (!startValue) {
            assert(false && "Failed to generate start value for for loop");
            return nullptr;
        }
        if (startValue->getType() != llvmVarType) {
            startValue = llvmState.Builder->CreateIntCast(startValue, llvmVarType, true, "for_start_cast");
        }
        Variable->addIncoming(startValue, PreheaderBB);
        llvmState.currentBreakBlock.currentLoop = LoopBB;
        llvmState.currentBreakBlock.afterLoop = AfterBB;
        // Generate the loop body.
        for (auto &exp: node->block()) {
            llvmState.Builder->SetInsertPoint(LoopBB);

            codegen_base(exp.get(), llvmState);
        }

        // Step: increment the loop variable.
        const auto stepValue = llvm::ConstantInt::get(llvmVarType, 1);
        const auto nextVar = llvmState.Builder->CreateAdd(Variable, stepValue, "nextvar");
        // Compute the end condition.
        auto endValue = codegen_base(node->rangeEnd(), llvmState);
        if (!endValue) {
            assert(false && "Failed to generate end value for for loop");
            return nullptr;
        }
        if (endValue->getType() != llvmVarType) {
            endValue = llvmState.Builder->CreateIntCast(endValue, llvmVarType, true, "for_end_cast");
        }
        llvm::Value *endCond = nullptr;
        if (node->inclusive()) {
            endCond = llvmState.Builder->CreateICmpSLE(nextVar, endValue, "loopcond");
        } else {
            endCond = llvmState.Builder->CreateICmpSLT(nextVar, endValue, "loopcond");
        }
        llvm::BasicBlock *loopEndBB = llvmState.Builder->GetInsertBlock();

        // Create the "after loop" block and insert it.
        llvmState.Builder->CreateCondBr(endCond, LoopBB, AfterBB);
        llvmState.Builder->SetInsertPoint(AfterBB);
        // Add the incoming value for the PHI node from the backedge.
        Variable->addIncoming(nextVar, loopEndBB);
        return llvm::Constant::getNullValue(llvm::Type::getInt32Ty(*llvmState.TheContext));
    }

    llvm::Value *codegen(ast::ForLoop *node, LLVMBackendState &llvmState) {
        if (!node->rangeEnd())
            return codegen_iterator_for(node, llvmState);

        return codegen_range_for(node, llvmState);
    }


    llvm::Value *codegen(ast::Comparisson *node, LLVMBackendState &llvmState) {
        auto lhs = codegen_base(node->lhs(), llvmState);
        assert(lhs && "lhs of the comparison is null");
        auto rhs = codegen_base(node->rhs(), llvmState);
        assert(rhs && "rhs of the comparison is null");

        llvm::CmpInst::Predicate pred = llvm::CmpInst::ICMP_EQ;
        if (lhs->getType()->isDoubleTy() || lhs->getType()->isFloatTy()) {
            pred = llvm::CmpInst::FCMP_OEQ;
            switch (node->cmpoperator()) {
                case ast::CMPOperator::NOT_EQUALS:
                    pred = llvm::CmpInst::FCMP_ONE;
                    break;
                case ast::CMPOperator::EQUALS:

                    break;
                case ast::CMPOperator::GREATER:
                    pred = llvm::CmpInst::FCMP_OGT;
                    break;
                case ast::CMPOperator::GREATER_EQUAL:
                    pred = llvm::CmpInst::FCMP_OGE;
                    break;
                case ast::CMPOperator::LESS:
                    pred = llvm::CmpInst::FCMP_OLT;
                    break;
                case ast::CMPOperator::LESS_EQUAL:
                    pred = llvm::CmpInst::FCMP_OLE;
                    break;
                default:
                    break;
            }
        } else {
            switch (node->cmpoperator()) {
                case ast::CMPOperator::NOT_EQUALS:
                    pred = llvm::CmpInst::ICMP_NE;
                    break;
                case ast::CMPOperator::EQUALS:

                    break;
                case ast::CMPOperator::GREATER:
                    pred = llvm::CmpInst::ICMP_SGT;
                    break;
                case ast::CMPOperator::GREATER_EQUAL:
                    pred = llvm::CmpInst::ICMP_SGE;
                    break;
                case ast::CMPOperator::LESS:
                    pred = llvm::CmpInst::ICMP_SLT;
                    break;
                case ast::CMPOperator::LESS_EQUAL:
                    pred = llvm::CmpInst::ICMP_SLE;
                    break;
                default:
                    break;
            }
        }

        const auto lhsType = node->lhs()->expressionType().value();
        const auto rhsType = node->rhs()->expressionType().value();
        if (lhsType && rhsType) {
            if (lhsType->name() == rhsType->name() && lhsType->typeKind() == types::TypeKind::INT) {
                if (lhs->getType()->isIntegerTy() && rhs->getType()->isIntegerTy()) {
                    const unsigned maxBitWith =
                            std::max(lhs->getType()->getIntegerBitWidth(), rhs->getType()->getIntegerBitWidth());
                    if (maxBitWith != lhs->getType()->getIntegerBitWidth()) {
                        lhs = llvmState.Builder->CreateIntCast(
                            lhs, llvm::IntegerType::getIntNTy(*llvmState.TheContext, maxBitWith), true, "lhs_cast");
                    }
                    if (maxBitWith != rhs->getType()->getIntegerBitWidth()) {
                        rhs = llvmState.Builder->CreateIntCast(
                            rhs, llvm::IntegerType::getIntNTy(*llvmState.TheContext, maxBitWith), true, "rhs_cast");
                    }
                }
            }
        }
        return llvmState.Builder->CreateCmp(pred, lhs, rhs);
    }

    llvm::Value *codegen(ast::LogicalExpression *node, LLVMBackendState &llvmState) {
        const auto lhs = node->lhs();
        const auto rhs = node->rhs();
        switch (node->logical_operator()) {
            case ast::LogicalOperator::AND:
                return llvmState.Builder->CreateAnd(codegen_base(lhs, llvmState), codegen_base(rhs, llvmState));
            case ast::LogicalOperator::OR:
                return llvmState.Builder->CreateOr(codegen_base(lhs, llvmState), codegen_base(rhs, llvmState));
            case ast::LogicalOperator::NOT:
                return llvmState.Builder->CreateNot(codegen_base(rhs, llvmState));
            default:
                assert(false && "unknown logical operator");
        }
    }

    llvm::Value *codegen(ast::WhileLoop *node, LLVMBackendState &llvmState) {
        llvm::Function *TheFunction = llvmState.Builder->GetInsertBlock()->getParent();

        llvm::BasicBlock *CondBB = llvm::BasicBlock::Create(*llvmState.TheContext, "loopcond", TheFunction);
        llvm::BasicBlock *LoopBB = llvm::BasicBlock::Create(*llvmState.TheContext, "loop", TheFunction);
        llvm::BasicBlock *AfterBB = llvm::BasicBlock::Create(*llvmState.TheContext, "afterloop", TheFunction);

        llvmState.Builder->CreateBr(CondBB);

        llvmState.Builder->SetInsertPoint(CondBB);
        auto condition = codegen_base(node->condition(), llvmState);
        if (!condition) {
            assert(false && "Failed to generate condition for while loop");
            return nullptr;
        }
        condition = llvmState.Builder->CreateICmpEQ(condition, llvmState.Builder->getTrue(), "whilecond");

        llvmState.Builder->CreateCondBr(condition, LoopBB, AfterBB);

        llvmState.Builder->SetInsertPoint(LoopBB);

        for (auto &exp: node->block()) {
            codegen_base(exp.get(), llvmState);
        }

        llvmState.Builder->CreateBr(CondBB);

        llvmState.Builder->SetInsertPoint(AfterBB);

        return AfterBB;
    }

    llvm::Value *codegen(ast::IfCondition *node, LLVMBackendState &llvmState) {
        auto condition = codegen_base(node->condition(), llvmState);
        if (!condition) {
            assert(false && "Failed to generate condition for if statement");
            return nullptr;
        }
        condition = llvmState.Builder->CreateICmpEQ(condition, llvmState.Builder->getTrue(), "ifcond");

        llvm::Function *TheFunction = llvmState.Builder->GetInsertBlock()->getParent();
        llvm::BasicBlock *ThenBB = llvm::BasicBlock::Create(*llvmState.TheContext, "then", TheFunction);
        const bool hasElse = !node->elseBlock().empty();
        llvm::BasicBlock *ElseBB = (hasElse) ? llvm::BasicBlock::Create(*llvmState.TheContext, "else") : nullptr;

        llvm::BasicBlock *MergeBB = llvm::BasicBlock::Create(*llvmState.TheContext, "ifcont");

        if (hasElse)
            llvmState.Builder->CreateCondBr(condition, ThenBB, ElseBB);
        else
            llvmState.Builder->CreateCondBr(condition, ThenBB, MergeBB);

        llvmState.Builder->SetInsertPoint(ThenBB);
        bool containsBreak = false;
        for (auto &exp: node->ifBlock()) {
            codegen_base(exp.get(), llvmState);
            if (auto brk = dynamic_cast<ast::BreakStatement *>(exp.get())) {
                containsBreak = true;
            }
        }

        if (!containsBreak)
            llvmState.Builder->CreateBr(MergeBB);
        //context->breakBlock().BlockUsed = false;
        if (ElseBB) {
            TheFunction->insert(TheFunction->end(), ElseBB);
            llvmState.Builder->SetInsertPoint(ElseBB);

            for (auto &exp: node->elseBlock()) {
                codegen_base(exp.get(), llvmState);
            }
            llvmState.Builder->CreateBr(MergeBB);
            // Codegen of 'Else' can change the current block, update ElseBB for the PHI.
            ElseBB = llvmState.Builder->GetInsertBlock();
        }


        // Emit merge block.
        TheFunction->insert(TheFunction->end(), MergeBB);
        llvmState.Builder->SetInsertPoint(MergeBB);


        return condition;
    }

    llvm::Value *codegen(ast::BinaryExpression *node, LLVMBackendState &llvmState) {
        const auto lhs = codegen_base(node->lhs(), llvmState);
        const auto rhs = codegen_base(node->rhs(), llvmState);

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
        std::string name = node->expressionToken().lexical();
        auto &NamedValues = llvmState.NamedValues;
        auto &NamedAllocations = llvmState.NamedAllocations;

        if (NamedValues.contains(name)) {
            auto value = NamedValues[name];
            if (auto globalVar = llvm::dyn_cast<llvm::GlobalVariable>(value)) {
                return llvmState.Builder->CreateLoad(globalVar->getValueType(), globalVar, name);
            }
            return value;
        }
        if (NamedAllocations.contains(name)) {
            return llvmState.Builder->CreateLoad(NamedAllocations[name]->getAllocatedType(), NamedAllocations[name],
                                                 name);
        }
        return nullptr;
    }

    llvm::Value *codegen(ast::VariableAssignment *node, LLVMBackendState &llvmState) {
        const auto alloca = llvmState.NamedAllocations.at(node->expressionToken().lexical());
        if (!alloca) {
            assert(false && "Variable not declared before assignment");
            return nullptr;
        }
        llvmState.Builder->CreateStore(codegen_base(node->expression(), llvmState), alloca);


        return alloca; // Return the allocation instruction
    }

    llvm::Value *codegen(const ast::VariableDeclaration *node, LLVMBackendState &llvmState) {
        llvm::Type *varType = resolveLlvmType(node->expressionType().value(), llvmState);

        if (node->constant()) {
            llvmState.NamedValues[node->expressionToken().lexical()] = nullptr;
            // Handle constant variable declaration if needed
            if (const auto initialValue = node->initialValue()) {
                if (llvm::Value *initValue = codegen_base(initialValue.value(), llvmState)) {
                    llvmState.NamedValues[node->expressionToken().lexical()] = initValue;
                } else {
                    assert(false && "Failed to generate initial value for constant");
                    return nullptr;
                }
            }
            return llvmState.NamedValues[node->expressionToken().lexical()];
        }


        llvm::AllocaInst *alloca = llvmState.Builder->CreateAlloca(varType, nullptr, node->expressionToken().lexical());
        llvmState.NamedAllocations[node->expressionToken().lexical()] = alloca;

        if (const auto initialValue = node->initialValue()) {
            if (llvm::Value *initValue = codegen_base(initialValue.value(), llvmState)) {
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
        const auto functionCall = llvmState.TheModule->getFunction(node->functionName());
        assert(functionCall && "Function not declared before call");
        if (!functionCall) {
            return nullptr; // Error handling
        }
        std::vector<llvm::Value *> args;
        for (const auto &arg: node->args()) {
            if (auto value = codegen_base(arg.get(), llvmState)) {
                args.push_back(value);
            } else {
                return nullptr; // Error handling
            }
        }
        return llvmState.Builder->CreateCall(functionCall, args, "funcCall");
    }

    llvm::Value *codegen(ast::ReturnStatement *node, LLVMBackendState &llvmState) {
        if (auto returnValue = node->returnValue()) {
            llvm::Value *retValue = llvm_backend::codegen_base(returnValue.value(), llvmState);
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
            auto paramType = resolveLlvmType(param.type.value(), llvmState);
            params.push_back(paramType);
        }
        llvm::FunctionType *FT = llvm::FunctionType::get(resultType, params, false);
        auto linkage = llvm::Function::ExternalLinkage;
        if (node->functionName() != "main") {
            linkage = llvm::Function::PrivateLinkage;
        }


        auto functionDefinition = llvm::Function::Create(FT, linkage, node->functionName(),
                                                         llvmState.TheModule.get());

        llvm::BasicBlock *functionBaseBlock = llvm::BasicBlock::Create(*llvmState.TheContext,
                                                                       functionDefinition->getName(),
                                                                       functionDefinition);

        llvmState.Builder->SetInsertPoint(functionBaseBlock);
        std::vector<std::string> argNames;
        for (auto &arg: functionDefinition->args()) {
            arg.setName(node->args()[arg.getArgNo()].name);
            // Create an alloca for this variable.
            llvm::AllocaInst *alloca = llvmState.Builder->CreateAlloca(arg.getType(), nullptr, arg.getName() + ".addr");
            // Store the initial value into the alloca.
            llvmState.Builder->CreateStore(&arg, alloca);
            // Add arguments to variable symbol table.
            llvmState.NamedAllocations[std::string(arg.getName())] = alloca;
            argNames.push_back(arg.getName().str());
        }

        for (auto &stmt: node->statements()) {
            llvm_backend::codegen_base(stmt.get(), llvmState);
        }
        functionDefinition->addFnAttr(llvm::Attribute::MustProgress);
        for (auto &argName: argNames) {
            llvmState.NamedAllocations.erase(argName);
        }


        llvm::AttrBuilder b(*llvmState.TheContext);
        b.addAttribute("frame-pointer", "all");
        functionDefinition->addFnAttrs(b);
        // if (!m_returnType)
        // {
        //     llvmState.TheBuilder->CreateRetVoid();
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
            context.TheMPM->addPass(llvm::createModuleToFunctionPassAdaptor(
                llvm::DCEPass())); // Remove dead functions and global variables.
        }
        // how do i remove unused functions?
        // Register analysis passes used in these transform passes.
        llvm::PassBuilder PB;
        const auto TheLAM = std::make_unique<llvm::LoopAnalysisManager>();
        const auto TheCGAM = std::make_unique<llvm::CGSCCAnalysisManager>();

        PB.registerModuleAnalyses(*context.TheMAM);
        PB.registerFunctionAnalyses(*context.TheFAM);
        PB.crossRegisterProxies(*TheLAM, *context.TheFAM, *TheCGAM, *context.TheMAM);
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
    context.TheModule->setDataLayout(TheTargetMachine->createDataLayout());

    createPrintfCall(*context.TheContext, *context.TheModule);

    for (auto &node: nodes) {
        if (auto funcDef = dynamic_cast<ast::FunctionDefinition *>(node.get())) {
            llvm_backend::codegen(funcDef, context);
        }
    }
    if (!llvm::verifyModule(*context.TheModule, &llvm::errs())) {
        if (options.buildMode == compiler::BuildMode::Release) {
            context.TheMPM->run(*context.TheModule, *context.TheMAM);
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
        //flags.erase(std::ranges::find(flags, "-lc"));
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
