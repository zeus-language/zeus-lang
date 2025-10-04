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
#include "ast/ArrayAssignment.h"
#include "ast/ArrayInitializer.h"
#include "ast/BinaryExpression.h"
#include "ast/BreakStatement.h"
#include "ast/Comparisson.h"
#include "ast/ExternFunctionDefinition.h"
#include "ast/FieldAccess.h"
#include "ast/FieldAssignment.h"
#include "ast/ForLoop.h"
#include "ast/FunctionCallNode.h"
#include "ast/IfCondition.h"
#include "ast/LogicalExpression.h"
#include "ast/NumberConstant.h"
#include "ast/ReturnStatement.h"
#include "ast/StringConstant.h"
#include "ast/StructInitialization.h"
#include "ast/TypeCast.h"
#include "ast/VariableAccess.h"
#include "ast/VariableAssignment.h"
#include "ast/VariableDeclaration.h"
#include "ast/WhileLoop.h"

namespace llvm_backend {
    struct BreakBlock {
        llvm::BasicBlock *afterLoop = nullptr;
        llvm::BasicBlock *currentLoop = nullptr;
        bool BlockUsed = false;
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


        llvm::Value *findVariable(const std::string &name, bool loadValue = true) {
            if (NamedValues.contains(name)) {
                return NamedValues[name];
            }
            if (NamedAllocations.contains(name)) {
                if (!loadValue) return NamedAllocations[name];
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

    llvm::Type *resolveLlvmType(const std::shared_ptr<types::VariableType> &value, LLVMBackendState &context);

    llvm::Type *resolveStructType(const std::shared_ptr<types::StructType> &structType, LLVMBackendState &context) {
        const auto typeName = structType->name();
        const auto cached_type = llvm::StructType::getTypeByName(*context.TheContext, typeName);
        if (cached_type == nullptr) {
            std::vector<llvm::Type *> types;
            for (const auto &[type, _]: structType->fields()) {
                types.emplace_back(resolveLlvmType(type, context));
            }

            const llvm::ArrayRef<llvm::Type *> elements(types);


            return llvm::StructType::create(*context.TheContext, elements, typeName);
        }
        return cached_type;
    }

    llvm::Type *resolveLlvmType(const std::shared_ptr<types::VariableType> &value, LLVMBackendState &context) {
        assert(value != nullptr && "Type is null");
        if (const auto &intType = std::dynamic_pointer_cast<types::IntegerType>(value)) {
            //return llvm::Type::getIntNTy(*context.TheContext, intType->size() * 8);
            return llvm::IntegerType::get(*context.TheContext, intType->size() * 8);
        }
        if (const auto &structType = std::dynamic_pointer_cast<types::StructType>(value)) {
            return resolveStructType(structType, context);
        }

        switch (value->typeKind()) {
            case types::TypeKind::INT:
                break;
            case types::TypeKind::FLOAT:
                return context.Builder->getFloatTy();
            case types::TypeKind::DOUBLE:
                return context.Builder->getDoubleTy();
            case types::TypeKind::STRING:
                return llvm::PointerType::getUnqual(*context.TheContext);
            case types::TypeKind::BOOL:
                return llvm::Type::getInt1Ty(*context.TheContext);
            case types::TypeKind::VOID:
                return llvm::Type::getVoidTy(*context.TheContext);
            case types::TypeKind::STRUCT:
                break;
            case types::TypeKind::ARRAY:
                if (const auto &arrayType = std::dynamic_pointer_cast<types::ArrayType>(value)) {
                    const auto baseType = resolveLlvmType(arrayType->baseType(), context);
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

    llvm::GlobalVariable *getOrCreateGlobalString(const LLVMBackendState &llvmState, const std::string &value,
                                                  const std::string &name) {
        auto _name = name;
        if (_name.empty()) {
            const auto hash = std::hash<std::string>{}(value);
            _name = "string." + std::to_string(hash);
        }

        if (const auto var = llvmState.TheModule->getGlobalVariable(_name, true))
            return var;
        return llvmState.Builder->CreateGlobalString(value, _name);
    }


    llvm::Value *codegen(const ast::ReturnStatement *node, LLVMBackendState &llvmState);

    llvm::Value *codegen(ast::FunctionCallNode *node, LLVMBackendState &llvmState);

    llvm::Value *codegen(const ast::NumberConstant *node, const LLVMBackendState &llvmState);

    llvm::Value *codegen(const ast::StringConstant *node, const LLVMBackendState &llvmState);

    llvm::Value *codegen(const ast::VariableDeclaration *node, LLVMBackendState &llvmState);

    llvm::Value *codegen(const ast::VariableAssignment *node, LLVMBackendState &llvmState);

    llvm::Value *codegen(const ast::VariableAccess *node, LLVMBackendState &llvmState);

    llvm::Value *codegen(const ast::BinaryExpression *node, LLVMBackendState &llvmState);

    llvm::Value *codegen(ast::IfCondition *node, LLVMBackendState &llvmState);

    llvm::Value *codegen(const ast::WhileLoop *node, LLVMBackendState &llvmState);

    llvm::Value *codegen(const ast::LogicalExpression *node, LLVMBackendState &llvmState);

    llvm::Value *codegen(const ast::Comparisson *node, LLVMBackendState &llvmState);

    llvm::Value *codegen(const ast::ForLoop *node, LLVMBackendState &llvmState);

    llvm::Value *codegen(ast::BreakStatement *, LLVMBackendState &llvmState);

    llvm::Value *codegen(const ast::ArrayInitializer *node, LLVMBackendState &llvmState);

    llvm::Value *codegen(const ast::ArrayAccess *node, LLVMBackendState &llvmState);

    llvm::Value *codegen(const ast::ArrayAssignment *node, LLVMBackendState &llvmState);

    llvm::Value *codegen(const ast::TypeCast *node, LLVMBackendState &llvmState);

    llvm::Value *codegen(const ast::StructInitialization *node, LLVMBackendState &llvmState);

    llvm::Value *codegen(const ast::FieldAccess *node, LLVMBackendState &llvmState);

    llvm::Value *codegen(const ast::FieldAssignment *node, LLVMBackendState &llvmState);

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
        if (const auto arrayAssign = dynamic_cast<ast::ArrayAssignment *>(node)) {
            return llvm_backend::codegen(arrayAssign, llvmState);
        }
        if (const auto typeCast = dynamic_cast<ast::TypeCast *>(node)) {
            return llvm_backend::codegen(typeCast, llvmState);
        }
        if (const auto structInit = dynamic_cast<ast::StructInitialization *>(node)) {
            return llvm_backend::codegen(structInit, llvmState);
        }
        if (const auto fieldAccess = dynamic_cast<ast::FieldAccess *>(node)) {
            return llvm_backend::codegen(fieldAccess, llvmState);
        }
        if (const auto fieldAssign = dynamic_cast<ast::FieldAssignment *>(node)) {
            return llvm_backend::codegen(fieldAssign, llvmState);
        }

        // Handle other node types or throw an error
        assert(false && "Unknown AST node type for code generation");
        return nullptr; // Placeholder
    }

    llvm::Value *codegen(const ast::FieldAssignment *node, LLVMBackendState &llvmState) {
        const auto type = std::dynamic_pointer_cast<types::StructType>(node->structType().value());
        const llvm::DataLayout &DL = llvmState.TheModule->getDataLayout();
        const auto value = codegen_base(node->expression(), llvmState);
        const auto element = codegen_base(node->accessNode(), llvmState);
        const auto elementPointer = getLoadStorePointerOperand(element);


        if (node->accessNode()->expressionType().value()->typeKind() == types::TypeKind::STRUCT) {
            const auto fieldtype = resolveLlvmType(node->accessNode()->expressionType().value(), llvmState);
            const size_t size = DL.getTypeAllocSize(fieldtype);
            llvmState.Builder->CreateMemCpy(elementPointer,
                                            llvm::MaybeAlign(),
                                            value,
                                            llvm::MaybeAlign(),
                                            size);
        } else {
            llvmState.Builder->CreateStore(value, elementPointer);
        }

        return value;
    }


    llvm::Value *codegen(const ast::FieldAccess *node, LLVMBackendState &llvmState) {
        const auto fieldName = node->fieldName().lexical();
        const auto recordType = std::dynamic_pointer_cast<types::StructType>(node->structType().value());
        const auto type = resolveLlvmType(recordType, llvmState);
        const auto structValue = codegen_base(node->accessNode(), llvmState);
        const auto structPointer = (!structValue->getType()->isPointerTy())
                                       ? getLoadStorePointerOperand(structValue)
                                       : structValue;

        const auto index = recordType->getFieldIndexByName(fieldName);
        const auto field = recordType->field(fieldName);

        const auto arrayValue = llvmState.Builder->CreateStructGEP(type, structPointer, index, fieldName);
        return llvmState.Builder->CreateLoad(resolveLlvmType(field->type, llvmState), arrayValue, fieldName);
    }

    llvm::Value *codegen(const ast::StructInitialization *node, LLVMBackendState &llvmState) {
        bool isConstantInitialisation = true;

        const auto structType = resolveLlvmType(node->expressionType().value(), llvmState);
        const auto type = std::dynamic_pointer_cast<types::StructType>(node->expressionType().value());
        std::vector<llvm::Constant *> fields;
        std::vector<llvm::Value *> values;
        for (const auto &[name, value]: node->fields()) {
            auto llvmValue = codegen_base(value.get(), llvmState);
            values.push_back(llvmValue);


            if (!llvm::isa<llvm::Constant>(llvmValue)) {
                isConstantInitialisation = false;
            } else {
                fields.push_back(llvm::cast<llvm::Constant>(llvmValue));
            }
        }
        if (isConstantInitialisation) {
            const llvm::ArrayRef<llvm::Constant *> tmp{fields};
            llvm::Constant *structDef = llvm::ConstantStruct::get(llvm::cast<llvm::StructType>(structType), tmp);
            auto *structValue = new llvm::GlobalVariable(*llvmState.TheModule,
                                                         structType,
                                                         true,
                                                         llvm::GlobalValue::PrivateLinkage,
                                                         structDef,
                                                         node->expressionToken().lexical()
            );
            return structValue;
        }
        const auto val = llvmState.Builder->CreateAlloca(structType);
        const llvm::DataLayout &DL = llvmState.TheModule->getDataLayout();
        for (size_t i = 0; i < values.size(); i++) {
            const auto [fieldType, fieldName] = type->fields()[i];
            const auto elementPointer =
                    llvmState.Builder->CreateStructGEP(structType, val, i, fieldName);
            const auto llvmFieldType = resolveLlvmType(fieldType, llvmState);
            const auto alignment = DL.getPrefTypeAlign(llvmFieldType);
            if (fieldType->typeKind() == types::TypeKind::STRUCT) {
                const auto value = (values[i]->getType()->isPointerTy())
                                       ? values[i]
                                       : getLoadStorePointerOperand(values[i]);

                const size_t size = DL.getTypeAllocSize(llvmFieldType);
                llvmState.Builder->CreateMemCpy(elementPointer,
                                                llvm::MaybeAlign(),
                                                value,
                                                llvm::MaybeAlign(),
                                                size);
            } else {
                llvmState.Builder->CreateAlignedStore(values[i], elementPointer, alignment);
            }
        }
        return llvmState.Builder->CreateLoad(structType, val);
    }

    llvm::Value *codegen(const ast::TypeCast *node, LLVMBackendState &llvmState) {
        const auto value = codegen_base(node->value(), llvmState);
        if (!value) {
            assert(false && "Unknown type");
        }
        const auto targetType = resolveLlvmType(node->expressionType().value(), llvmState);
        if (!targetType) {
            assert(false && "Unknown target type");
        }
        if (value->getType() == targetType) {
            return value; // No cast needed
        }
        if (targetType->isIntegerTy()) {
            if (value->getType()->isFloatingPointTy()) {
                return llvmState.Builder->CreateFPToSI(value, targetType, "float_to_int_cast");
            }
            return llvmState.Builder->CreateIntCast(value, targetType, true, "int_cast");
        }
        if (targetType->isFloatingPointTy()) {
            if (value->getType()->isIntegerTy()) {
                return llvmState.Builder->CreateSIToFP(value, targetType, "int_to_float_cast");
            }
            return llvmState.Builder->CreateFPCast(value, targetType, "float_cast");
        }
        if (targetType->isArrayTy() && value->getType()->isPointerTy()) {
            return value;
        }
        if (targetType->isPointerTy() && value->getType()->isIntegerTy()) {
            return llvmState.Builder->CreateIntToPtr(value, targetType, "int_to_ptr_cast");
        }

        assert(false && "Unsupported type cast");
        return nullptr; // Placeholder
    }

    std::shared_ptr<types::VariableType> resolveBaseType(const std::shared_ptr<types::VariableType> &type) {
        if (const auto &arrayType = std::dynamic_pointer_cast<types::ArrayType>(type)) {
            return arrayType->baseType();
        }
        if (const auto &ptrType = std::dynamic_pointer_cast<types::PointerType>(type)) {
            return ptrType->baseType();
        }
        assert(false && "Type is not an array or pointer");
        return nullptr;
    }

    llvm::Value *codegen(const ast::ArrayAssignment *node, LLVMBackendState &llvmState) {
        auto baseType = node->arrayType();

        const auto arrayPtr = llvmState.findVariable(node->expressionToken().lexical(),
                                                     baseType->typeKind() == types::TypeKind::POINTER);

        const auto arrayType = resolveLlvmType(baseType, llvmState);
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
        if (baseType->typeKind() == types::TypeKind::ARRAY) {
            indices.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(*llvmState.TheContext), 0));
        }

        if (indexValue->getType() != llvm::Type::getInt32Ty(*llvmState.TheContext)) {
            indexValue = llvmState.Builder->CreateIntCast(indexValue, llvm::Type::getInt32Ty(*llvmState.TheContext),
                                                          true, "array_index_cast");
        }
        indices.push_back(indexValue);
        const auto valueToStore = codegen_base(node->value(), llvmState);
        if (!valueToStore) {
            assert(false && "Failed to generate value for array assignment");
            return nullptr;
        }
        llvm::Value *elementPtr = nullptr;
        if (baseType->typeKind() == types::TypeKind::ARRAY) {
            elementPtr = llvmState.Builder->CreateGEP(arrayType, arrayPtr, indices, "elem_ptr");
        } else {
            auto ptrType = std::dynamic_pointer_cast<types::PointerType>(baseType);
            if (!ptrType) {
                assert(false && "Array assignment base type is not a pointer");
            }
            auto type = resolveLlvmType(ptrType->baseType(), llvmState);
            elementPtr = llvmState.Builder->CreateGEP(type, arrayPtr, indices, "elem_ptr");
        }
        llvmState.Builder->CreateStore(valueToStore, elementPtr);
        return valueToStore;
    }

    llvm::Value *codegen(const ast::ArrayInitializer *node, LLVMBackendState &llvmState) {
        if (node->elements().empty()) {
            return llvm::Constant::getNullValue(llvm::ArrayType::get(
                llvm::Type::getInt8Ty(*llvmState.TheContext), 0));
        } else {
            std::vector<llvm::Constant *> elements;
            for (auto &element: node->elements()) {
                const auto elemValue = codegen_base(element.get(), llvmState);
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
            const auto firstElemType = elements[0]->getType();
            for (const auto &elem: elements) {
                if (elem->getType() != firstElemType) {
                    assert(false && "Array initializer elements have different types");
                    return nullptr;
                }
            }
            const auto arrayType = llvm::ArrayType::get(firstElemType, elements.size());
            const auto arrayConstant = llvm::ConstantArray::get(arrayType, elements);
            const auto hash = std::hash<std::string>{}(node->expressionToken().lexical());
            const std::string arrayName = "array." + std::to_string(hash);
            const auto arrayVar = new llvm::GlobalVariable(
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

    llvm::Value *codegen(const ast::ArrayAccess *node, LLVMBackendState &llvmState) {
        const auto arrayPtr = llvmState.findVariable(node->expressionToken().lexical());
        auto baseType = node->arrayType().value();
        const auto arrayType = resolveLlvmType(baseType, llvmState);
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
        const auto zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*llvmState.TheContext), 0);
        indices.push_back(zero);
        if (indexValue->getType() != llvm::Type::getInt32Ty(*llvmState.TheContext)) {
            indexValue = llvmState.Builder->CreateIntCast(indexValue, llvm::Type::getInt32Ty(*llvmState.TheContext),
                                                          true, "array_index_cast");
        }
        indices.push_back(indexValue);


        const auto elementPtr = llvmState.Builder->CreateGEP(arrayType, arrayPtr, indices, "elem_ptr");
        const auto loadedValue = llvmState.Builder->CreateLoad(arrayType->getArrayElementType(),
                                                               elementPtr,
                                                               "array_elem");
        return loadedValue;
    }

    llvm::Value *codegen(ast::BreakStatement *, LLVMBackendState &llvmState) {
        llvmState.currentBreakBlock.BlockUsed = true;
        return llvmState.Builder->CreateBr(llvmState.currentBreakBlock.afterLoop);
    }

    llvm::Value *codegen_iterator_for(const ast::ForLoop *node, LLVMBackendState &llvmState) {
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
        const auto llvmVarType = resolveLlvmType(varType.value(), llvmState);
        if (!llvmVarType) {
            assert(false && "Could not resolve LLVM type of iterator variable in for loop");
            return nullptr;
        }

        llvm::PHINode *Variable = llvmState.Builder->CreatePHI(llvmVarType, 2);


        const auto iterableValue = codegen_base(node->rangeStart(), llvmState);
        const auto iterableType = node->rangeStart()->expressionType();
        if (!iterableValue) {
            assert(false && "Failed to generate iterable value for a for loop");
            return nullptr;
        }
        size_t arraySize = 0;
        if (const auto &arrayType = std::dynamic_pointer_cast<types::ArrayType>(iterableType.value())) {
            arraySize = arrayType->size();
        } else {
            assert(false && "Iterable in for loop is not an array");
            return nullptr;
        }

        llvm::Value *startValue = llvmState.Builder->getInt32(0);

        // load array value
        const auto arrayType = resolveLlvmType(node->rangeStart()->expressionType().value(), llvmState);
        const auto arrayAllocation = llvmState.findVariable(node->rangeStart()->expressionToken().lexical());

        if (startValue->getType() != llvmVarType) {
            startValue = llvmState.Builder->CreateIntCast(startValue, llvmVarType, true, "for_start_cast");
        } {
            std::vector<llvm::Value *> indices;
            const auto zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*llvmState.TheContext), 0);
            indices.push_back(zero);
            indices.push_back(Variable);
            const auto elementPtr = llvmState.Builder->CreateGEP(arrayType, arrayAllocation, indices,
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
        llvmState.currentBreakBlock.BlockUsed = false;
        // Generate the loop body.
        llvmState.Builder->SetInsertPoint(LoopBB);
        for (auto &exp: node->block()) {
            codegen_base(exp.get(), llvmState);
        }
        llvmState.currentBreakBlock.currentLoop = nullptr;
        llvmState.currentBreakBlock.afterLoop = nullptr;
        // Step: increment the loop variable.
        const auto stepValue = llvm::ConstantInt::get(llvmVarType, 1);
        const auto nextVar = llvmState.Builder->CreateAdd(Variable, stepValue, "nextvar");

        Variable->addIncoming(startValue, PreheaderBB);

        // Compute the end condition.
        llvm::Value *endValue = llvmState.Builder->getInt32(arraySize);
        if (!endValue) {
            assert(false && "Failed to generate end value for the for loop");
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

    llvm::Value *codegen_range_for(const ast::ForLoop *node, LLVMBackendState &llvmState) {
        llvm::Function *TheFunction = llvmState.Builder->GetInsertBlock()->getParent();

        llvm::BasicBlock *PreheaderBB = llvmState.Builder->GetInsertBlock();
        llvm::BasicBlock *LoopBB = llvm::BasicBlock::Create(*llvmState.TheContext, "for.body", TheFunction);
        llvm::BasicBlock *AfterBB = llvm::BasicBlock::Create(*llvmState.TheContext, "for.cleanup", TheFunction);

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
        const auto llvmVarType = resolveLlvmType(varType.value(), llvmState);
        if (!llvmVarType) {
            assert(false && "Could not resolve LLVM type of iterator variable in for loop");
            return nullptr;
        }

        llvm::PHINode *Variable = llvmState.Builder->CreatePHI(llvmVarType, 2, node->iteratorToken().lexical());
        llvmState.NamedValues[node->iteratorToken().lexical()] = Variable;

        // Initialize the PHI node with the start value.
        auto startValue = codegen_base(node->rangeStart(), llvmState);
        if (!startValue) {
            assert(false && "Failed to generate start value for the for loop");
            return nullptr;
        }
        if (startValue->getType() != llvmVarType) {
            startValue = llvmState.Builder->CreateIntCast(startValue, llvmVarType, true, "for_start_cast");
        }
        Variable->addIncoming(startValue, PreheaderBB);
        llvmState.currentBreakBlock.afterLoop = AfterBB;
        llvmState.currentBreakBlock.BlockUsed = false;
        // Generate the loop body.
        llvmState.Builder->SetInsertPoint(LoopBB);

        for (auto &exp: node->block()) {
            codegen_base(exp.get(), llvmState);
        }
        llvmState.currentBreakBlock.currentLoop = nullptr;
        llvmState.currentBreakBlock.afterLoop = nullptr;

        // Step: increment the loop variable.
        const auto stepValue = llvm::ConstantInt::get(llvmVarType, 1);
        const auto nextVar = llvmState.Builder->CreateAdd(Variable, stepValue, "nextvar");
        // Compute the end condition.
        auto endValue = codegen_base(node->rangeEnd(), llvmState);
        if (!endValue) {
            assert(false && "Failed to generate end value for the for loop");
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

    llvm::Value *codegen(const ast::ForLoop *node, LLVMBackendState &llvmState) {
        if (!node->rangeEnd())
            return codegen_iterator_for(node, llvmState);

        return codegen_range_for(node, llvmState);
    }


    llvm::Value *codegen(const ast::Comparisson *node, LLVMBackendState &llvmState) {
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

    llvm::Value *codegen(const ast::LogicalExpression *node, LLVMBackendState &llvmState) {
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

    llvm::Value *codegen(const ast::WhileLoop *node, LLVMBackendState &llvmState) {
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
        llvmState.currentBreakBlock.BlockUsed = false;
        for (auto &exp: node->block()) {
            codegen_base(exp.get(), llvmState);
        }
        llvmState.currentBreakBlock.BlockUsed = false;
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

        for (auto &exp: node->ifBlock()) {
            codegen_base(exp.get(), llvmState);
        }

        if (!llvmState.currentBreakBlock.BlockUsed)
            llvmState.Builder->CreateBr(MergeBB);


        if (hasElse) {
            TheFunction->insert(TheFunction->end(), ElseBB);
            llvmState.Builder->SetInsertPoint(ElseBB);

            for (auto &exp: node->elseBlock()) {
                codegen_base(exp.get(), llvmState);
            }
            if (!llvmState.currentBreakBlock.BlockUsed)
                llvmState.Builder->CreateBr(MergeBB);
        } else {
            llvmState.currentBreakBlock.BlockUsed = false;
        }


        // Emit merge block.
        TheFunction->insert(TheFunction->end(), MergeBB);
        llvmState.Builder->SetInsertPoint(MergeBB);


        return condition;
    }

    llvm::Value *codegen(const ast::BinaryExpression *node, LLVMBackendState &llvmState) {
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
            case ast::BinaryOperator::MOD:
                if (lhs->getType()->isIntegerTy() && rhs->getType()->isIntegerTy()) {
                    return llvmState.Builder->CreateSRem(lhs, rhs, "modtmp");
                }
                break;
            default:
                assert(false && "Unsupported binary operator");
        }
        assert(false && "Type mismatch in binary expression");
    }


    llvm::Value *codegen(const ast::VariableAccess *node, LLVMBackendState &llvmState) {
        const std::string name = node->expressionToken().lexical();
        auto &NamedValues = llvmState.NamedValues;
        auto &NamedAllocations = llvmState.NamedAllocations;

        if (NamedValues.contains(name)) {
            const auto value = NamedValues[name];
            if (const auto globalVar = llvm::dyn_cast<llvm::GlobalVariable>(value)) {
                return llvmState.Builder->CreateLoad(globalVar->getValueType(), globalVar, name);
            }
            return value;
        }
        if (NamedAllocations.contains(name)) {
            if (NamedAllocations[name]->getAllocatedType()->isArrayTy() or NamedAllocations[name]->getAllocatedType()->
                isStructTy()) {
                return NamedAllocations[name];
            }
            return llvmState.Builder->CreateLoad(NamedAllocations[name]->getAllocatedType(), NamedAllocations[name],
                                                 name);
        }
        return nullptr;
    }

    llvm::Value *codegen(const ast::VariableAssignment *node, LLVMBackendState &llvmState) {
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

        if (node->constant() && !varType->isStructTy()) {
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
            if (const auto stringConstant = dynamic_cast<ast::StringConstant *>(initialValue.value())) {
                const auto strValue = codegen(stringConstant, llvmState);

                llvmState.Builder->CreateMemCpy(llvmState.NamedAllocations[node->expressionToken().lexical()],
                                                llvm::MaybeAlign(),
                                                strValue,
                                                llvm::MaybeAlign(),
                                                stringConstant->expressionToken().lexical().size() + 1);
                return alloca;
            } else if (const auto structInit = dynamic_cast<ast::StructInitialization *>(initialValue.value())) {
                const auto strValue = codegen(structInit, llvmState);
                const llvm::DataLayout &DL = llvmState.TheModule->getDataLayout();
                const size_t size = DL.getTypeAllocSize(varType);
                const auto value = (strValue->getType()->isPointerTy())
                                       ? strValue
                                       : getLoadStorePointerOperand(strValue);

                llvmState.Builder->CreateMemCpy(llvmState.NamedAllocations[node->expressionToken().lexical()],
                                                llvm::MaybeAlign(),
                                                value,
                                                llvm::MaybeAlign(),
                                                size);
                return alloca;
            }
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
            } else if (varType->isStructTy()) {
                // do nothing may be later initialize it with zero
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
                                              llvm::APInt(node->numBits(), std::get<int64_t>(node->value()), true));
            case ast::NumberType::FLOAT:
                return llvm::ConstantFP::get(llvmState.Builder->getFloatTy(), std::get<double>(node->value()));
            case ast::NumberType::CHAR:
                return llvm::ConstantInt::get(*llvmState.TheContext,
                                              llvm::APInt(8, static_cast<uint64_t>(std::get<int64_t>(node->value())),
                                                          false));
        }
        assert(false && "unknown number type in codegen");
        return nullptr;
    }

    llvm::Value *codegen(const ast::StringConstant *node, const LLVMBackendState &llvmState) {
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
                } else if (value->getType()->isFloatingPointTy()) {
                    args.push_back(getOrCreateGlobalString(llvmState, "%f\n", "double_format"));
                } else if (value->getType()->isPointerTy() || value->getType()->isArrayTy()) {
                    args.push_back(getOrCreateGlobalString(llvmState, "%s\n", "string_format"));
                } else {
                    assert(false && "Unsupported argument type for println");
                    return nullptr;
                }
                if (value->getType()->isFloatingPointTy()) {
                    args.push_back(
                        llvmState.Builder->CreateFPCast(value, llvmState.Builder->getDoubleTy(), "float_to_double"));
                } else {
                    args.push_back(value);
                }
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
        const auto call = llvmState.Builder->CreateCall(functionCall, args);
        if (node->functionName() == "malloc" || node->functionName() == "calloc")
            call->addRetAttr(llvm::Attribute::NoAlias);
        for (size_t i = 0; i < node->args().size(); ++i) {
            const auto argType = node->args()[i]->expressionType();

            if (argType.has_value() && argType.value()->typeKind() == types::TypeKind::STRUCT) {
                const auto llvmArgType = resolveLlvmType(argType.value(), llvmState);

                call->addParamAttr(static_cast<unsigned>(i), llvm::Attribute::NoUndef);
                call->addParamAttr(static_cast<unsigned>(i),
                                   llvm::Attribute::getWithByValType(*llvmState.TheContext, llvmArgType));
            }
        }
        if (!functionCall->getReturnType()->isVoidTy())
            return call;
        return nullptr;
    }

    llvm::Value *codegen(const ast::ReturnStatement *node, LLVMBackendState &llvmState) {
        llvmState.currentBreakBlock.BlockUsed = true;
        if (const auto returnValue = node->returnValue()) {
            llvm::Value *retValue = llvm_backend::codegen_base(returnValue.value(), llvmState);
            return llvmState.Builder->CreateRet(retValue);
        } else {
            return llvmState.Builder->CreateRetVoid();
        }
    }

    void codegen(ast::ExternFunctionDefinition *node, LLVMBackendState &llvmState) {
        const auto resultType = resolveLlvmType(node->expressionType().value(), llvmState);
        std::vector<llvm::Type *> params;
        for (const auto &param: node->args()) {
            if (param.type.value()->typeKind() == types::TypeKind::STRUCT) {
                params.push_back(llvm::PointerType::getUnqual(*llvmState.TheContext));
            } else {
                params.push_back(resolveLlvmType(param.type.value(), llvmState));
            }
        }
        llvm::FunctionType *FT = llvm::FunctionType::get(resultType, params, false);
        auto linkage = llvm::Function::ExternalLinkage;


        auto functionDefinition = llvm::Function::Create(FT, linkage, node->functionName(),
                                                         llvmState.TheModule.get());


        std::vector<std::string> argNames;
        for (auto &arg: functionDefinition->args()) {
            auto &param = node->args()[arg.getArgNo()];
            arg.setName(param.name);
        }
        //attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

        if (functionDefinition->getName() == "malloc" || functionDefinition->getName() == "calloc") {
            functionDefinition->addRetAttr(llvm::Attribute::NoAlias);
            functionDefinition->addFnAttr(llvm::Attribute::NoFree);
            functionDefinition->addFnAttr(llvm::Attribute::NoUnwind);
            functionDefinition->addFnAttr(llvm::Attribute::WillReturn);
            functionDefinition->addFnAttr(llvm::Attribute::Speculatable);
            functionDefinition->addFnAttr(llvm::Attribute::Memory);
            functionDefinition->addFnAttr(llvm::Attribute::NoSync);
            functionDefinition->addFnAttr(llvm::Attribute::NoCallback);
        }


        if (llvm::verifyFunction(*functionDefinition, &llvm::errs())) {
            llvmState.TheFPM->run(*functionDefinition, *llvmState.TheFAM);
        }
    }

    void codegen(ast::FunctionDefinition *node, LLVMBackendState &llvmState) {
        // Placeholder for actual LLVM IR generation logic
        const auto resultType = resolveLlvmType(node->expressionType().value(), llvmState);
        std::vector<llvm::Type *> params;
        for (const auto &param: node->args()) {
            if (param.type.value()->typeKind() == types::TypeKind::STRUCT) {
                params.push_back(llvm::PointerType::getUnqual(*llvmState.TheContext));
            } else {
                params.push_back(resolveLlvmType(param.type.value(), llvmState));
            }
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
            auto &param = node->args()[arg.getArgNo()];
            arg.setName(param.name);
            // Create an alloca for this variable.
            const auto varType = resolveLlvmType(param.type.value(), llvmState);
            llvm::AllocaInst *alloca = llvmState.Builder->CreateAlloca(varType,
                                                                       nullptr, arg.getName() + ".addr");
            // Store the initial value into the alloca.
            if (param.type.value()->typeKind() == types::TypeKind::STRUCT) {
                const llvm::DataLayout &DL = llvmState.TheModule->getDataLayout();
                const size_t size = DL.getTypeAllocSize(varType);
                llvmState.Builder->CreateMemCpy(alloca,
                                                llvm::MaybeAlign(),
                                                &arg,
                                                llvm::MaybeAlign(),
                                                size);
            } else {
                llvmState.Builder->CreateStore(&arg, alloca);
            }
            // Add arguments to variable symbol table.
            llvmState.NamedAllocations[std::string(arg.getName())] = alloca;
            argNames.push_back(arg.getName().str());
            if (param.type.value()->typeKind() == types::TypeKind::STRUCT) {
                arg.addAttr(llvm::Attribute::getWithByValType(*llvmState.TheContext,
                                                              resolveLlvmType(param.type.value(), llvmState)));
                arg.addAttr(llvm::Attribute::NoUndef);
            }
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
        if (auto funcDef = dynamic_cast<ast::ExternFunctionDefinition *>(node.get())) {
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
    if (options.printLLVMIR) {
        context.TheModule->print(llvm::errs(), nullptr, false, false);
    }
    pass.run(*context.TheModule);
    dest.flush();
    dest.close();


    llvm::verifyModule(*context.TheModule, &llvm::errs());


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
