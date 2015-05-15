#include <iostream>
#include <fstream>
#include <sstream>
#include <memory>
#include <exception>
#include <vector>
#include <map>
#include <algorithm>
#include <cstdlib>
#include <cctype>

#include <llvm/Analysis/Passes.h>
#include <llvm/Bitcode/ReaderWriter.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/PassManager.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Transforms/Scalar.h>

namespace kaleidoscope
{
    template<typename T>
    class symtab
    {
        std::vector<std::unique_ptr<std::map<std::string, std::unique_ptr<T>>>> scopes_;
    public:
        void enter_scope()
        {
            std::cout << "+scope" << std::endl;
            scopes_.push_back(std::make_unique<std::map<std::string, std::unique_ptr<T>>>());
        }
        
        void exit_scope()
        {
            scopes_.pop_back();
            std::cout << "-scope" << std::endl;
        }
        
        void insert(const std::string &key, std::unique_ptr<T> value)
        {
            std::cout << "registering " << key << std::endl;
            scopes_.back()->emplace(key, std::move(value));
        }
        
        bool lookup(const std::string &key, T &out) const
        {
            for (auto &&s : scopes_) {
                auto it = s->find(key);
                if (it != s->end()) {
                    out = *it->second;
                    return true;
                }
            }
            return false;
        }
    };
    
    class symbol
    {
        llvm::Value *val_;
        llvm::Type *type_;
    public:
        symbol(llvm::Value *val = nullptr, llvm::Type *type = nullptr) : val_(val), type_(type) { }
        
        virtual ~symbol() { }
        
        llvm::Value *value() const
        {
            return val_;
        }
        
        llvm::Type *type() const
        {
            return type_;
        }
    };
    
    class param_sym : public symbol
    {
        unsigned int idx_ = 0;
    public:
        param_sym(llvm::Value *val, llvm::Type *type, unsigned int idx) : symbol(val, type), idx_(idx) { }
        
        unsigned int index() const
        {
            return idx_;
        }
    };
    
    class field_sym : public symbol
    {
        unsigned int idx_ = 0;
    public:
        field_sym(llvm::Type *type, unsigned int idx) : symbol(nullptr, type), idx_(idx) { }
        
        unsigned int index() const
        {
            return idx_;
        }
    };
    
    struct codegen_ctx
    {
        llvm::Module module_;
        // llvm::FunctionPassManager pass_mgr_;
        llvm::IRBuilder<> builder_;
        llvm::Function *curr_fun_;
        
        symtab<symbol> symtab_;
        std::map<std::string, std::unique_ptr<field_sym>> fieldtab_;
        std::map<std::string, llvm::Type *> tenv_;
        
        codegen_ctx(const std::string &name)
        : module_{name, llvm::getGlobalContext()}, builder_{module_.getContext()}
        { }
        
        void init()
        {
            symtab_.enter_scope();
            
            tenv_["int8"] = llvm::Type::getInt8Ty(module_.getContext());
            tenv_["int16"] = llvm::Type::getInt16Ty(module_.getContext());
            tenv_["int32"] = llvm::Type::getInt32Ty(module_.getContext());
            tenv_["int64"] = llvm::Type::getInt64Ty(module_.getContext());
            tenv_["double"] = llvm::Type::getDoubleTy(module_.getContext());
            
            // llvm::InitializeNativeTarget();
            
            // pass_mgr_.addPass(llvm::createBasicAliasAnalysisPass());
            // pass_mgr_.addPass(llvm::createPromoteMemoryToRegisterPass());
            // pass_mgr_.addPass(llvm::createInstructionCombiningPass());
            // pass_mgr_.addPass(llvm::createReassociatePass());
            // pass_mgr_.addPass(llvm::createGVNPass());
            // pass_mgr_.addPass(llvm::createCFGSimplificationPass());
            
            // pass_mgr_.doInitialization();
        }
    };
    
    llvm::Value *load(codegen_ctx &ctx, llvm::Value *addr)
    {
        return ctx.builder_.CreateLoad(addr);
    }
    
    llvm::Value *store(codegen_ctx &ctx, llvm::Value *addr, llvm::Value *value)
    {
        return ctx.builder_.CreateStore(value, addr);
    }
    
    // Types
    
    llvm::Type *resolve_type(const codegen_ctx &ctx, const std::string &name)
    {
        auto it = ctx.tenv_.find(name);
        if (it == ctx.tenv_.end())
            throw std::domain_error("unknown type " + name);
        return it->second;
    }
    
    // AST
    
    class expr_node
    {
    public:
        virtual ~expr_node() { }
        
        virtual llvm::Type *type(codegen_ctx &ctx) = 0;
        virtual llvm::Value *codegen(codegen_ctx &ctx) = 0;

        virtual bool is_assignable() const
        {
            return false;
        }

        virtual bool is_qualified() const
        {
            return false;
        }

        virtual std::string name() const
        {
            return "";
        }
    };
    
    class number_node : public expr_node
    {
        int value_;
    public:
        number_node(int value) : value_(value) { }
        
        int value() const
        {
            return value_;
        }
        
        llvm::Type *type(codegen_ctx &ctx)
        {
            return resolve_type(ctx, "int32");
        }
        
        llvm::Value *codegen(codegen_ctx &ctx)
        {
            return llvm::ConstantInt::get(ctx.module_.getContext(), llvm::APInt(32, value_));
        }
    };
    
    class var_node : public expr_node
    {
        std::string name_;
    public:
        var_node(const std::string &name) : name_(name) { }
        
        std::string name() const
        {
            return name_;
        }
        
        llvm::Type *type(codegen_ctx &ctx)
        {
            symbol sym;
            if (!ctx.symtab_.lookup(name_, sym))
                throw std::domain_error("unbound variable " + name_);
            return sym.type();
        }
        
        llvm::Value *codegen(codegen_ctx &ctx)
        {
            symbol sym;
            ctx.symtab_.lookup(name_, sym);
            return sym.value();
        }

        bool is_assignable() const
        {
            return true;
        }
    };
    
    class var_decl_node : public expr_node
    {
        std::unique_ptr<expr_node> dest_;
        std::unique_ptr<expr_node> value_;
    public:
        var_decl_node(std::unique_ptr<expr_node> dest, std::unique_ptr<expr_node> value) : dest_(std::move(dest)), value_(std::move(value)) { }
        
        llvm::Type *type(codegen_ctx &ctx)
        {
            if (!dest_->is_assignable())
                throw std::domain_error("lhs in assignment is not assignable");
            auto value_ty = value_->type(ctx);
            if (!dest_->is_qualified())
                ctx.symtab_.insert(dest_->name(), std::make_unique<symbol>(nullptr, value_ty));
            return value_ty;
        }
        
        llvm::Value *codegen(codegen_ctx &ctx)
        {
            if (!dest_->is_assignable())
                throw std::domain_error("lhs in assignment is not assignable");
            auto value = value_->codegen(ctx);
            llvm::IRBuilder<> builder(&ctx.curr_fun_->getEntryBlock(), ctx.curr_fun_->getEntryBlock().begin());

            if (!dest_->is_qualified()) {
                auto addr = builder.CreateAlloca(value->getType(), 0, dest_->name().c_str());
                store(ctx, addr, value);
                ctx.symtab_.insert(dest_->name(), std::make_unique<symbol>(value, value->getType()));
                return addr;
            }

            throw std::domain_error("not implemented: qualified lhs in assignment");
        }
        
    };
    
    class field_access_node : public expr_node
    {
        std::unique_ptr<expr_node> target_;
        std::string field_;
    public:
        field_access_node(std::unique_ptr<expr_node> target, std::string field)
        : target_(std::move(target)), field_(field)
        { }
        
        llvm::Type *type(codegen_ctx &ctx)
        {
            auto target_ty = target_->type(ctx)->getSequentialElementType();
            if (!target_ty->isStructTy())
                throw std::domain_error("trying to index a non-struct");
            auto target_ty_name = target_ty->getStructName().str();
            auto name = target_ty_name + "." + field_;
            auto it = ctx.fieldtab_.find(name);
            if (it == ctx.fieldtab_.end())
                throw std::domain_error("unbound field " + field_);
            return it->second->type();
        }
        
        llvm::Value *codegen(codegen_ctx &ctx)
        {
            auto target_ty = target_->type(ctx)->getSequentialElementType();
            auto target_ty_name = target_ty->getStructName().str();
            auto name = target_ty_name + "." + field_;
            auto it = ctx.fieldtab_.find(name);
            if (it == ctx.fieldtab_.end())
                throw std::domain_error("unbound field " + field_);
            auto field_ptr = ctx.builder_.CreateStructGEP(target_->codegen(ctx), it->second->index());
            return load(ctx, field_ptr);
        }

        bool is_assignable() const
        {
            return true;
        }

        bool is_qualified() const
        {
            return true;
        }
    };
    
    class binary_op_node : public expr_node
    {
        char op_;
        std::unique_ptr<expr_node> lhs_;
        std::unique_ptr<expr_node> rhs_;
    public:
        binary_op_node(char op, std::unique_ptr<expr_node> lhs, std::unique_ptr<expr_node> rhs)
        : op_(op), lhs_(std::move(lhs)), rhs_(std::move(rhs))
        { }
        
        llvm::Type *type(codegen_ctx &ctx)
        {
            return resolve_type(ctx, "int32");
        }
        
        llvm::Value *codegen(codegen_ctx &ctx)
        {
            auto lval = lhs_->codegen(ctx);
            auto rval = rhs_->codegen(ctx);
            
            switch (op_) {
                case '+':
                    return ctx.builder_.CreateFAdd(lval, rval, "addtmp");
                case '-':
                    return ctx.builder_.CreateFSub(lval, rval, "subtmp");
            }
            
            throw std::domain_error("unknown operator");
        }
    };
    
    class call_node : public expr_node
    {
        std::unique_ptr<expr_node> target_;
        std::vector<std::unique_ptr<expr_node>> args_;
    public:
        call_node(std::unique_ptr<expr_node> target, std::vector<std::unique_ptr<expr_node>> args)
        : target_(std::move(target)), args_(std::move(args))
        { }
        
        llvm::Type *type(codegen_ctx &ctx)
        {
            auto fun_ty = target_->type(ctx);
            if (!fun_ty->isFunctionTy())
                throw std::domain_error("trying to call a non-function");
            return llvm::cast<llvm::FunctionType>(fun_ty)->getReturnType();
        }
        
        llvm::Value *codegen(codegen_ctx &ctx)
        {
            auto f = llvm::cast<llvm::Function>(target_->codegen(ctx));
            auto is_ctor = std::isupper(f->getName().str()[0]);
            
            if (!is_ctor && f->arg_size() != args_.size())
                 throw std::domain_error("argument count mismatch");
            else if (is_ctor && f->arg_size() != args_.size() + 1)
                throw std::domain_error("argument count mismatch");
            
            std::vector<llvm::Value *> argvs;
            auto arg_it = begin(args_);
            for (auto param_it = f->arg_begin(); param_it != f->arg_end(); ++param_it) {
                auto param_ty = param_it->getType();
                if (param_ty->isPointerTy() && is_ctor && param_it == f->arg_begin()) {
                    argvs.push_back(ctx.builder_.CreateAlloca(param_ty->getSequentialElementType()));
                } else {
                    const std::unique_ptr<expr_node> &expr = *arg_it++;
                    argvs.push_back(expr->codegen(ctx));
                }
            }
            
            return ctx.builder_.CreateCall(f, argvs, "calltmp");
        }
    };
    
    class seq_node : public expr_node
    {
        std::vector<std::unique_ptr<expr_node>> exprs_;
    public:
        void add_expr(std::unique_ptr<expr_node> expr)
        {
            exprs_.push_back(std::move(expr));
        }
        
        llvm::Type *type(codegen_ctx &ctx)
        {
            // XXX not necessary to keep all values, just the last
            std::vector<llvm::Type *> types(exprs_.size());
            std::transform(begin(exprs_), end(exprs_), begin(types), [&](auto &&expr) { return expr->type(ctx); });
            return types.back();
        }
        
        llvm::Value *codegen(codegen_ctx &ctx)
        {
            // XXX not necessary to keep all values, just the last
            std::vector<llvm::Value *> values(exprs_.size());
            std::transform(begin(exprs_), end(exprs_), begin(values), [&](auto &&expr) { return expr->codegen(ctx); });
            return values.back();
        }
    };
    
    class def_node
    {
    protected:
        llvm::Type *type_;
        
    public:
        virtual ~def_node() { }
        
        virtual llvm::Type *type(codegen_ctx &ctx) = 0;
        
        // XXX This is too restrictive, e.g. type defs don't return Function *
        virtual llvm::Function *codegen(codegen_ctx &ctx) = 0;
    };
    
    class struct_node : public def_node
    {
        std::string name_;
        std::vector<std::pair<std::string, std::string>> fields_;
        
        llvm::Function *ctor_fun_;
    public:
        struct_node(std::string name, std::vector<std::pair<std::string, std::string>> fields)
        : name_(name), fields_(fields)
        { }
        
        llvm::Type *type(codegen_ctx &ctx)
        {
            std::vector<llvm::Type *> field_tys(fields_.size());
            std::transform(begin(fields_), end(fields_), begin(field_tys), [&](auto field) {
                auto field_ty = resolve_type(ctx, field.second);
                if (field_ty->isAggregateType())
                    field_ty = llvm::PointerType::getUnqual(field_ty);
                return field_ty;
            });
            auto struct_ty = llvm::StructType::create(ctx.module_.getContext(), field_tys, "struct." + name_);
            ctx.tenv_[name_] = struct_ty;
            
            type_ = struct_ty;
            
            auto idx = 0;
            for (auto &&field : fields_) {
                auto name = "struct." +  name_ + "." + field.first;
                ctx.fieldtab_[name] = std::make_unique<field_sym>(field_tys[idx], idx);
                idx++;
            }
            
            auto struct_ty_ptr = llvm::PointerType::getUnqual(type_);
            field_tys.insert(field_tys.begin(), struct_ty_ptr);
            
            auto fun_ty = llvm::FunctionType::get(struct_ty_ptr, field_tys, false);
            ctor_fun_ = llvm::Function::Create(fun_ty, llvm::Function::ExternalLinkage, name_, &ctx.module_);
            ctx.symtab_.insert(name_, std::make_unique<symbol>(ctor_fun_, fun_ty));
            
            return type_;
        }
        
        llvm::Function *codegen(codegen_ctx &ctx)
        {
            assert(type_ != nullptr);
            
            // ctx.enter_scope();
            
            auto block = llvm::BasicBlock::Create(ctx.module_.getContext(), "entry", ctor_fun_);
            ctx.builder_.SetInsertPoint(block);
            
            auto ret_val = ctor_fun_->arg_begin();
            
            unsigned idx = 0;
            auto iter = ctor_fun_->arg_begin();
            while (++iter != ctor_fun_->arg_end()) {
                if (iter->getType()->isPointerTy()) {
                    llvm::AttributeSet attrs;
                    iter->addAttr(attrs.addAttribute(ctx.module_.getContext(), 0, llvm::Attribute::ByVal));
                }
                auto field_ptr = ctx.builder_.CreateStructGEP(ret_val, idx++);
                store(ctx, field_ptr, iter);
            }
            
            ctx.builder_.CreateRet(ret_val);
            
            llvm::verifyFunction(*ctor_fun_);
            // ctx.pass_mgr_.run(*ctor_fun_);
            
            // ctx.exit_scope();
            
            return ctor_fun_;
        }
    };
    
    class prototype_node : public def_node
    {
        std::string name_;
        std::vector<std::pair<std::string, std::string>> params_;
        std::string ty_annot_;
        
    public:
        prototype_node(std::string name, std::vector<std::pair<std::string, std::string>> params, std::string ty_annot)
        : name_(name), params_(params), ty_annot_(ty_annot)
        { }
        
        llvm::Type *type(codegen_ctx &ctx)
        {
            std::vector<llvm::Type *> param_tys(params_.size());
            std::transform(begin(params_), end(params_), begin(param_tys), [&](auto param) {
                auto ty = resolve_type(ctx, param.second);
                if (ty->isAggregateType())
                    ty = llvm::PointerType::getUnqual(ty);
                return ty;
            });
            
            auto ret_ty = resolve_type(ctx, ty_annot_);
            if (ret_ty->isAggregateType())
                ret_ty = llvm::PointerType::getUnqual(ret_ty);
            
            type_ = llvm::FunctionType::get(ret_ty, param_tys, false);
            auto f = llvm::Function::Create(static_cast<llvm::FunctionType *>(type_), llvm::Function::ExternalLinkage, name_, &ctx.module_);
            ctx.symtab_.insert(name_, std::make_unique<symbol>(f, type_));
            
            return type_;
        }
        
        llvm::Function *codegen(codegen_ctx &ctx)
        {
            assert(type_ != nullptr && type_->isFunctionTy());
            auto f = ctx.module_.getFunction(name_);
            assert(f != nullptr);
            
            if (f->getName() != name_) {
                f->eraseFromParent();
                f = ctx.module_.getFunction(name_);
            }
            
            if (!f->empty())
                throw std::domain_error("function redefinition");
            
            if (f->arg_size() != params_.size())
                throw std::domain_error("function redefinition with different number of params");
            
            unsigned idx = 0;
            for (auto iter = f->arg_begin(); iter != f->arg_end(); ++iter, ++idx) {
                auto name = params_[idx].first;
                iter->setName(name);
                if (iter->getType()->isPointerTy()) {
                    llvm::AttributeSet attrs;
                    iter->addAttr(attrs.addAttribute(ctx.module_.getContext(), 0, llvm::Attribute::ByVal));
                }
                ctx.symtab_.insert(name, std::make_unique<param_sym>(iter, type_->getFunctionParamType(idx), idx));
            }
            
            return f;
        }
    };
    
    class function_node : public def_node
    {
        std::unique_ptr<prototype_node> proto_;
        std::unique_ptr<expr_node> body_;
    public:
        function_node(std::unique_ptr<prototype_node> proto, std::unique_ptr<expr_node> body)
        : proto_(std::move(proto)), body_(std::move(body))
        { }
        
        llvm::Type *type(codegen_ctx &ctx)
        {
            return proto_->type(ctx);
        }
        
        llvm::Function *codegen(codegen_ctx &ctx)
        {
            ctx.symtab_.enter_scope();
            
            auto f = proto_->codegen(ctx);
            
            ctx.curr_fun_ = f;
            
            auto block = llvm::BasicBlock::Create(ctx.module_.getContext(), "entry", f);
            ctx.builder_.SetInsertPoint(block);
            
            auto ret_val = body_->codegen(ctx);
            ctx.builder_.CreateRet(ret_val);
            
            llvm::verifyFunction(*f);
            // ctx.pass_mgr_.run(*f);
            
            ctx.curr_fun_ = nullptr;
            
            ctx.symtab_.exit_scope();
            
            return f;
        }
    };
    
    class module_node
    {
        std::vector<std::unique_ptr<def_node>> defs_;
    public:
        void add_def(std::unique_ptr<def_node> def)
        {
            defs_.push_back(std::move(def));
        }
        
        void type(codegen_ctx &ctx)
        {
            for (auto &&def : defs_)
                def->type(ctx);
        }
        
        void codegen(codegen_ctx &ctx)
        {
            for (auto &&def : defs_)
                def->codegen(ctx);
        }
    };
    
    // Scanner
    
    enum class token
    {
        unknown = -2,
        eof = -1,
        indent = 0,
        dedent,
        ident,
        num,
        kw_struct,
        kw_def,
        kw_extern,
        rarr,
        eq,
        lpar,
        rpar,
        period,
        comma,
        colon,
        semicolon
    };
    
    std::string to_string(token tok)
    {
        switch (tok) {
            case token::unknown:
                return "unknown";
            case token::eof:
                return "end of file";
            case token::indent:
                return "indent";
            case token::dedent:
                return "dedent";
            case token::kw_struct:
                return "struct keyword";
            case token::kw_def:
                return "def keyword";
            case token::kw_extern:
                return "extern keyword";
            case token::ident:
                return "identifier";
            case token::num:
                return "number";
            case token::rarr:
                return "->";
            case token::eq:
                return "=";
            case token::lpar:
                return "(";
            case token::rpar:
                return ")";
            case token::period:
                return ".";
            case token::comma:
                return ",";
            case token::colon:
                return ":";
            case token::semicolon:
                return ";";
        }
        return "unknown";
    }

    class scanner
    {
        std::istream &in_;
        std::string lexeme_;

        std::vector<int> indents_ { 0 };
        int pending_dedents_ = 0;
        bool new_line_ = true;

    public:
        scanner(std::istream &in) : in_{in} { }

        std::string lexeme() const
        {
            return lexeme_;
        }
        
        template<typename UnaryPredicate>
        void read_while(std::istream &in, UnaryPredicate pred)
        {
            lexeme_.clear();
            while (!in.eof() && pred(in.peek()))
                lexeme_.push_back(in.get());
        }
        
        token next()
        {
            if (in_.eof())
                return token::eof;

            if (pending_dedents_ > 0) {
                pending_dedents_--;
                return token::dedent;
            }

            auto tok = token::unknown;
            lexeme_.clear();

            while (in_.peek() == '\n') {
                in_.get();
                new_line_ = true;
            }

            auto indent = 0;
            while (isspace(in_.peek())) {
                in_.get();
                if (new_line_)
                    indent++;
            }

            if (new_line_) {
                new_line_ = false;

                if (indent > indents_.back()) {
                    indents_.push_back(indent);
                    return token::indent;
                }

                if (indent < indents_.back()) {
                    while (indent < indents_.back()) {
                        pending_dedents_++;
                        indents_.pop_back();
                    }
                    pending_dedents_--;
                    return token::dedent;
                }
            }

            if (in_.peek() == '-') {
                in_.get();
                if (in_.peek() == '>') {
                    tok = token::rarr;
                    in_.get();
                }
            } else if (in_.peek() == '=') {
                tok = token::eq;
                in_.get();
            } else if (in_.peek() == '(') {
                tok = token::lpar;
                in_.get();
            } else if (in_.peek() == ')') {
                tok = token::rpar;
                in_.get();
            } else if (in_.peek() == '.') {
                tok = token::period;
                in_.get();
            } else if (in_.peek() == ',') {
                tok = token::comma;
                in_.get();
            } else if (in_.peek() == ':') {
                tok = token::colon;
                in_.get();
            } else if (in_.peek() == ';') {
                tok = token::semicolon;
                in_.get();
            } else if (isdigit(in_.peek())) {
                read_while(in_, isdigit);
                tok = token::num;
            } else if (isalnum(in_.peek())) {
                read_while(in_, [](auto c) { return isalnum(c) || c == '_'; });
                if (lexeme_ == "struct")
                    tok = token::kw_struct;
                else if (lexeme_ == "def")
                    tok = token::kw_def;
                else if (lexeme_ == "extern")
                    tok = token::kw_extern;
                else
                    tok = token::ident;
            } else {
                read_while(in_, [](auto c) { return !isspace(c); });
            }
            return tok;
        }
    };

    // Parser
    
    class parser
    {
    public:
        token curr_tok_;
        scanner scanner_;
        
    public:
        parser(std::istream &in) : scanner_{in} { }
        
        std::unique_ptr<module_node> parse()
        {
            move_next();
            return parse_module();
        }
        
        // module = { def }
        std::unique_ptr<module_node> parse_module()
        {
            auto mod = std::make_unique<module_node>();
            while (curr_tok_ != token::eof)
                switch (curr_tok_) {
                    case token::kw_struct:
                    case token::kw_extern:
                    case token::kw_def:
                        mod->add_def(parse_def());
                        break;
                    default:
                        throw std::domain_error("unexpected token " + to_string(curr_tok_));
                }
            return mod;
        }
        
        // def = fun_proto | fun_decl
        std::unique_ptr<def_node> parse_def()
        {
            switch (curr_tok_) {
                case token::kw_struct:
                    return parse_struct();
                case token::kw_extern:
                    move_next();
                    return parse_prototype(true);
                case token::kw_def:
                    return parse_function();
            }
            throw std::domain_error("expecting a definition at the toplevel");
        }
        
        // struct_decl = "struct" ident "{" annotated_ident { ";" annotated_ident } "}"
        std::unique_ptr<struct_node> parse_struct()
        {
            expect(token::kw_struct);
            auto name = scanner_.lexeme();
            move_next();

            expect(token::colon);

            std::vector<std::pair<std::string, std::string>> fields;

            expect(token::indent);
            
            fields.push_back(parse_annotated_ident());
            while (curr_tok_ != token::dedent)
                fields.push_back(parse_annotated_ident());

            while (curr_tok_ == token::dedent)
                move_next();
            
            return std::make_unique<struct_node>(name, std::move(fields));
        }
        
        // fun_decl = fun_proto fun_body
        // fun_body = "{" expr "}"
        std::unique_ptr<function_node> parse_function()
        {
            auto proto = parse_prototype();

            expect(token::indent);

            auto body = parse_expr_seq();

            while (curr_tok_ == token::dedent)
                move_next();

            return std::make_unique<function_node>(std::move(proto), std::move(body));
        }
        
        // fun_proto = "fun" ident "(" [ident {"," ident}] } ")" ":" ident
        std::unique_ptr<prototype_node> parse_prototype(bool is_extern = false)
        {
            expect(token::kw_def);
            auto name = scanner_.lexeme();
            move_next();
            
            expect(token::lpar);
            
            std::vector<std::pair<std::string, std::string>> params;
            
            if (curr_tok_ != token::rpar) {
                params.push_back(parse_annotated_ident());
                while (curr_tok_ == token::comma) {
                    move_next();
                    params.push_back(parse_annotated_ident());
                }
            }
            
            expect(token::rpar);
            expect(token::rarr);
            auto ty_annot = scanner_.lexeme();
            expect(token::ident);

            if (!is_extern)
                expect(token::colon);
            
            return std::make_unique<prototype_node>(name, std::move(params), ty_annot);
        }
        
        // annotated_ident = ident ":" ident
        std::pair<std::string, std::string> parse_annotated_ident()
        {
            auto name = scanner_.lexeme();
            expect(token::ident);
            expect(token::colon);
            auto ty_annot = scanner_.lexeme();
            expect(token::ident);
            return std::make_pair(name, ty_annot);
        }
        
        // expr_seq = expr { ";" expr }
        std::unique_ptr<expr_node> parse_expr_seq()
        {
            auto node = std::make_unique<seq_node>();
            node->add_expr(std::move(parse_expr()));
            while (curr_tok_ != token::dedent)
                node->add_expr(std::move(parse_expr()));
            return std::move(node);
        }
        
        // expr = factor | val_decl
        std::unique_ptr<expr_node> parse_expr()
        {
            auto node = parse_factor();
            if (curr_tok_ == token::eq) {
                move_next();
                auto value = parse_expr();
                return std::make_unique<var_decl_node>(std::move(node), std::move(value));
            }
            return std::move(node);
        }
        
        // factor = num | ident | call
        std::unique_ptr<expr_node> parse_factor()
        {
            auto tok = curr_tok_;
            switch (tok) {
                case token::num:
                {
                    auto value = std::stoi(scanner_.lexeme());
                    move_next();
                    return std::make_unique<number_node>(value);
                }
                case token::ident:
                    return parse_call();
                case token::lpar:
                {
                    move_next();
                    auto result = parse_expr();
                    expect(token::rpar);
                    return result;
                }
            }
            throw std::domain_error("unexpected expr " + to_string(curr_tok_));
        }
        
        // call = qualident "(" [expr {"," expr}] } ")"
        std::unique_ptr<expr_node> parse_call()
        {
            auto target = parse_ident();
            
            if (curr_tok_ != token::lpar)
                return target;
            
            expect(token::lpar);
            
            std::vector<std::unique_ptr<expr_node>> args;
            
            if (curr_tok_ != token::rpar) {
                args.push_back(parse_expr());
                while (curr_tok_ == token::comma) {
                    move_next();
                    args.push_back(parse_expr());
                }
            }
            
            expect(token::rpar);
            
            return std::make_unique<call_node>(std::move(target), std::move(args));
        }
        
        // qualident = ident { "." ident }
        std::unique_ptr<expr_node> parse_ident()
        {
            auto name = scanner_.lexeme();
            move_next();
            
            expr_node *result = new var_node(name);
            
            while (curr_tok_ == token::period) {
                move_next();
                auto field_name = scanner_.lexeme();
                move_next();
                result = new field_access_node(std::unique_ptr<expr_node>(result), field_name);
            }
            
            return std::unique_ptr<expr_node>(result);
        }
        
    private:
        void move_next()
        {
            curr_tok_ = scanner_.next();
        }
        
        token expect(token tok)
        {
            auto aux = curr_tok_;
            if (aux != tok) {
                std::cerr << "expecting " << to_string(tok) << ", but got " << to_string(aux) << " (" << scanner_.lexeme() << ")" << std::endl;
                throw std::domain_error(to_string(aux));
            }
            move_next();
            return aux;
        }
    };
}

std::string basename(const std::string& pathname)
{
    return std::string(std::find(rbegin(pathname), rend(pathname), '/').base(), end(pathname));
}

std::string remove_extension(const std::string& filename)
{
    auto pivot = std::find(rbegin(filename), rend(filename), '.');
    return pivot == filename.rend() ? filename : std::string(begin(filename), pivot.base() - 1);
}

int main(int argc, const char *argv[])
{
    std::ifstream in{argv[1]};
    
    if (!in.good()) {
        in.close();
        exit(1);
    }
    
    auto module_name = remove_extension(basename(argv[1]));
    kaleidoscope::codegen_ctx ctx{module_name};
    
    ctx.init();
    
    kaleidoscope::parser p{in};
    
    auto node = p.parse();
    node->type(ctx);
    node->codegen(ctx);
    
    ctx.module_.dump();
    
    std::string project_path = "./";
    
    auto bc_file_name = module_name + ".bc";
    auto obj_file_name = module_name + ".o";
    auto exe_file_name = module_name + ".out";
    
    std::error_code err;
    llvm::raw_fd_ostream os((project_path + bc_file_name).c_str(), err, llvm::sys::fs::F_None);
    llvm::WriteBitcodeToFile(&ctx.module_, os);
    os.close();
    
    system(("/usr/local/opt/llvm/bin/llc -filetype=obj " + project_path + bc_file_name + " -o " + project_path + obj_file_name).c_str());
    system(("clang -std=c11 -O1 -Wall -Werror -c " + project_path + "lib.c" + " -o "  + project_path + "lib.o").c_str());
    system(("clang " + project_path + "lib.o " + project_path + obj_file_name + " -o " + project_path + exe_file_name).c_str());
    
    return 0;
}
