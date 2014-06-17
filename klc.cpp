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
#include <llvm/Analysis/Verifier.h>
#include <llvm/Bitcode/ReaderWriter.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/PassManager.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Transforms/Scalar.h>

namespace kaleidoscope
{
    // Semantic

    class symbol
    {
        llvm::Value *val_;
        llvm::Type *type_;
    public:
        symbol(llvm::Value *val, llvm::Type *type) : val_(val), type_(type) { }

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

    // Codegen

    struct codegen_ctx
    {
        llvm::Module module_;
        llvm::FunctionPassManager pass_mgr_;
        llvm::IRBuilder<> builder_;
        std::map<std::string, std::unique_ptr<symbol>> symtab_;
        std::map<std::string, std::unique_ptr<field_sym>> fieldtab_;
        std::map<std::string, llvm::Type *> tenv_;

        codegen_ctx(const std::string &name)
        : module_{name, llvm::getGlobalContext()}, pass_mgr_{&module_}, builder_{module_.getContext()}
        { }

        void init()
        {
            tenv_["int8"] = llvm::Type::getInt8Ty(module_.getContext());
            tenv_["int16"] = llvm::Type::getInt16Ty(module_.getContext());
            tenv_["int32"] = llvm::Type::getInt32Ty(module_.getContext());
            tenv_["int64"] = llvm::Type::getInt64Ty(module_.getContext());
            tenv_["double"] = llvm::Type::getDoubleTy(module_.getContext());

            // llvm::InitializeNativeTarget();

            pass_mgr_.add(llvm::createBasicAliasAnalysisPass());
            pass_mgr_.add(llvm::createInstructionCombiningPass());
            pass_mgr_.add(llvm::createReassociatePass());
            pass_mgr_.add(llvm::createGVNPass());
            pass_mgr_.add(llvm::createCFGSimplificationPass());

            pass_mgr_.doInitialization();
        }

        void enter_scope() { }

        void exit_scope()
        {
            symtab_.clear();
        }
    };

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
            auto it = ctx.symtab_.find(name_);
            if (it == ctx.symtab_.end())
                throw std::domain_error("unbound variable");
            return it->second->type();
        }

        llvm::Value *codegen(codegen_ctx &ctx)
        {
            auto it = ctx.symtab_.find(name_);
            if (it == ctx.symtab_.end())
                throw std::domain_error("unbound variable");
            return it->second->value();
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
            auto it = ctx.fieldtab_.find(target_ty_name);
            if (it == ctx.fieldtab_.end())
                throw std::domain_error("unbound field");
            return it->second->type();
        }

        llvm::Value *codegen(codegen_ctx &ctx)
        {
            auto target_ty = target_->type(ctx)->getSequentialElementType();
            auto target_ty_name = target_ty->getStructName().str();
            auto name = target_ty_name + "." + field_;
            auto it = ctx.fieldtab_.find(name);
            if (it == ctx.fieldtab_.end())
                throw std::domain_error("unbound field");
            auto field_ptr = ctx.builder_.CreateStructGEP(target_->codegen(ctx), it->second->index());
            return ctx.builder_.CreateLoad(field_ptr);
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

        char op() const
        {
            return op_;
        }

        const expr_node *lhs() const
        {
            return lhs_.get();
        }

        const expr_node *rhs() const
        {
            return rhs_.get();
        }

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
        std::string target_;
        std::vector<std::unique_ptr<expr_node>> args_;
    public:
        call_node(std::string target, std::vector<std::unique_ptr<expr_node>> args)
        : target_(target), args_(std::move(args))
        { }

        std::string target() const
        {
            return target_;
        }

        const std::vector<std::unique_ptr<expr_node>> &args() const
        {
            return args_;
        }

        llvm::Type *type(codegen_ctx &ctx)
        {
            auto f = ctx.module_.getFunction(target_);
            if (!f)
                throw std::domain_error("unknown function");
            return f->getReturnType();
        }

        llvm::Value *codegen(codegen_ctx &ctx)
        {
            auto f = ctx.module_.getFunction(target_);
            if (!f)
                throw std::domain_error("unknown function");
            // if (f->arg_size() != args_.size())
            //     throw std::domain_error("argument count mismatch");
            auto has_hidden_param = f->arg_size() != args_.size();

            std::vector<llvm::Value *> argvs;
            auto arg_it = begin(args_);
            for (auto param_it = f->arg_begin(); param_it != f->arg_end(); ++param_it) {
                auto param_ty = param_it->getType();
                if (param_ty->isPointerTy() && has_hidden_param) {
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
    public:
        struct_node(std::string name, std::vector<std::pair<std::string, std::string>> fields)
        : name_(name), fields_(fields)
        { }

        llvm::Type *type(codegen_ctx &ctx)
        {
            std::vector<llvm::Type *> field_tys(fields_.size());
            std::transform(begin(fields_), end(fields_), begin(field_tys), [&](auto field) { return resolve_type(ctx, field.second); });
            auto struct_ty = llvm::StructType::create(ctx.module_.getContext(), field_tys, "struct." + name_);
            ctx.tenv_[name_] = struct_ty;

            auto idx = 0;
            for (auto &&field : fields_) {
                auto name = "struct." +  name_ + "." + field.first;
                ctx.fieldtab_[name] = std::make_unique<field_sym>(field_tys[idx], idx);
                idx++;
            }

            return type_ = struct_ty;
        }

        llvm::Function *codegen(codegen_ctx &ctx)
        {
            assert(type_ != nullptr);
            // ctor

            std::vector<llvm::Type *> param_tys;
            auto struct_ty_ptr = llvm::PointerType::getUnqual(type_);
            param_tys.push_back(struct_ty_ptr);

            for (unsigned i = 0; i < type_->getStructNumElements(); ++i)
                param_tys.push_back(type_->getStructElementType(i));

            auto fun_ty = llvm::FunctionType::get(struct_ty_ptr, param_tys, false);
            auto f = llvm::Function::Create(fun_ty, llvm::Function::ExternalLinkage, name_, &ctx.module_);

            // ctx.enter_scope();

            auto block = llvm::BasicBlock::Create(ctx.module_.getContext(), "entry", f);
            ctx.builder_.SetInsertPoint(block);

            auto ret_val = f->arg_begin();

            unsigned idx = 0;
            auto iter = f->arg_begin();
            while (++iter != f->arg_end()) {
                auto field_ptr = ctx.builder_.CreateStructGEP(ret_val, idx++);
                ctx.builder_.CreateStore(iter, field_ptr);
            }

            ctx.builder_.CreateRet(ret_val);

            llvm::verifyFunction(*f);
            ctx.pass_mgr_.run(*f);

            // ctx.exit_scope();

            return f;
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
            ctx.symtab_[name_] = std::make_unique<symbol>(f, type_);

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
                ctx.symtab_[name] = std::make_unique<param_sym>(iter, type_->getFunctionParamType(idx), idx);
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
            ctx.enter_scope();

            auto f = proto_->codegen(ctx);

            auto block = llvm::BasicBlock::Create(ctx.module_.getContext(), "entry", f);
            ctx.builder_.SetInsertPoint(block);

            auto ret_val = body_->codegen(ctx);
            ctx.builder_.CreateRet(ret_val);

            llvm::verifyFunction(*f);
            ctx.pass_mgr_.run(*f);

            ctx.exit_scope();

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
        ident = 0,
        num,
        kw_struct,
        kw_fun,
        kw_extern,
        lpar,
        rpar,
        lcurly,
        rcurly,
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
            case token::kw_struct:
                return "struct keyword";
            case token::kw_fun:
                return "fun keyword";
            case token::kw_extern:
                return "extern keyword";
            case token::ident:
                return "identifier";
            case token::num:
                return "number";
            case token::lpar:
                return "(";
            case token::rpar:
                return ")";
            case token::lcurly:
                return "{";
            case token::rcurly:
                return "}";
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

    static std::string lexeme;

    template<typename UnaryPredicate>
    void read_while(std::istream &in, UnaryPredicate pred)
    {
        lexeme.clear();
        while (!in.eof() && pred(in.peek()))
            lexeme.push_back(in.get());
    }

    token next(std::istream &in)
    {
        if (in.eof())
            return token::eof;
        auto tok = token::unknown;
        lexeme.clear();
        while (isspace(in.peek()))
            in.get();
        if (in.peek() == '(') {
            tok = token::lpar;
            in.get();
        } else if (in.peek() == ')') {
            tok = token::rpar;
            in.get();
        } else if (in.peek() == '{') {
            tok = token::lcurly;
            in.get();
        } else if (in.peek() == '}') {
            tok = token::rcurly;
            in.get();
        } else if (in.peek() == '.') {
            tok = token::period;
            in.get();
        } else if (in.peek() == ',') {
            tok = token::comma;
            in.get();
        } else if (in.peek() == ':') {
            tok = token::colon;
            in.get();
        } else if (in.peek() == ';') {
            tok = token::semicolon;
            in.get();
        } else if (isdigit(in.peek())) {
            read_while(in, isdigit);
            tok = token::num;
        } else if (isalnum(in.peek())) {
            read_while(in, [](auto c) { return isalnum(c) || c == '_'; });
            if (lexeme == "struct")
                tok = token::kw_struct;
            else if (lexeme == "fun")
                tok = token::kw_fun;
            else if (lexeme == "extern")
                tok = token::kw_extern;
            else
                tok = token::ident;
        } else {
            read_while(in, [](auto c) { return !isspace(c); });
        }
        return tok;
    }

    // Parser

    class parser
    {
    public:
        token curr_tok_;
        std::istream &in_;

    public:
        parser(std::istream &in) : in_(in) { }

        std::unique_ptr<module_node> parse()
        {
            move_next();
            return parse_module();
        }

        // module = { def }
        std::unique_ptr<module_node> parse_module()
        {
            auto mod = std::make_unique<module_node>();
            while (curr_tok_ == token::kw_struct || curr_tok_ == token::kw_extern || curr_tok_ == token::kw_fun)
                mod->add_def(parse_def());
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
                    return parse_prototype();
                case token::kw_fun:
                    return parse_function();
            }
            throw std::domain_error("expecting a definition at the toplevel");
        }

        // struct_decl = "struct" ident "{" annotated_ident { ";" annotated_ident } "}"
        std::unique_ptr<struct_node> parse_struct()
        {
            expect(token::kw_struct);
            auto name = lexeme;
            move_next();
            std::vector<std::pair<std::string, std::string>> fields;

            expect(token::lcurly);

            fields.push_back(parse_annotated_ident());
            while (curr_tok_ == token::semicolon) {
                move_next();
                fields.push_back(parse_annotated_ident());
            }

            expect(token::rcurly);

            return std::make_unique<struct_node>(name, std::move(fields));
        }

        // fun_decl = fun_proto fun_body
        // fun_body = "{" expr "}"
        std::unique_ptr<function_node> parse_function()
        {
            auto proto = parse_prototype();
            expect(token::lcurly);
            auto body = parse_expr_seq();
            expect(token::rcurly);
            return std::make_unique<function_node>(std::move(proto), std::move(body));
        }

        // fun_proto = "fun" ident "(" [ident {"," ident}] } ")" ":" ident
        std::unique_ptr<prototype_node> parse_prototype()
        {
            expect(token::kw_fun);
            auto name = lexeme;
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
            expect(token::colon);
            auto ty_annot = lexeme;
            expect(token::ident);

            return std::make_unique<prototype_node>(name, std::move(params), ty_annot);
        }

        // annotated_ident = ident ":" ident
        std::pair<std::string, std::string> parse_annotated_ident()
        {
            auto name = lexeme;
            expect(token::ident);
            expect(token::colon);
            auto ty_annot = lexeme;
            expect(token::ident);
            return std::make_pair(name, ty_annot);
        }

        // expr_seq = expr { ";" expr }
        std::unique_ptr<expr_node> parse_expr_seq()
        {
            auto node = std::make_unique<seq_node>();
            node->add_expr(std::move(parse_expr()));
            while (curr_tok_ == token::semicolon) {
                move_next();
                node->add_expr(std::move(parse_expr()));
            }
            return std::move(node);
        }

        // expr = factor
        std::unique_ptr<expr_node> parse_expr()
        {
            return parse_factor();
        }

        // factor = num | ident | call
        std::unique_ptr<expr_node> parse_factor()
        {
            auto tok = curr_tok_;
            switch (tok) {
                case token::num:
                {
                    auto value = std::stoi(lexeme);
                    move_next();
                    return std::make_unique<number_node>(value);
                }
                case token::ident:
                    return parse_ident();
                case token::lpar:
                {
                    move_next();
                    auto result = parse_expr();
                    expect(token::rpar);
                    return result;
                }
            }
            throw std::domain_error("unexpected expr");
        }

        // call = ident "(" [ident {"," ident}] } ")"
        std::unique_ptr<expr_node> parse_ident()
        {
            auto name = lexeme;
            move_next();

            switch (curr_tok_) {
                case token::lpar:
                {
                    move_next();
                    std::vector<std::unique_ptr<expr_node>> args;

                    if (curr_tok_ != token::rpar) {
                        args.push_back(parse_expr());
                        while (curr_tok_ == token::comma) {
                            move_next();
                            args.push_back(parse_expr());
                        }
                    }

                    expect(token::rpar);

                    return std::make_unique<call_node>(name, std::move(args));
                }
                break;
                case token::period:
                {
                    move_next();
                    auto field_name = lexeme;
                    move_next();
                    return std::make_unique<field_access_node>(std::make_unique<var_node>(name), field_name);
                }
                break;
            }

            return std::make_unique<var_node>(name);
        }

    private:
        void move_next()
        {
            curr_tok_ = next(in_);
        }

        token expect(token tok)
        {
            auto aux = curr_tok_;
            if (aux != tok) {
                std::cerr << "expecting " << to_string(tok) << ", but got " << to_string(aux) << "(" << lexeme << ")" << std::endl;
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
    auto module_name = remove_extension(basename(argv[1]));

    kaleidoscope::codegen_ctx ctx{module_name};
    ctx.init();

    std::ifstream in{argv[1]};
    kaleidoscope::parser p{in};

    auto node = p.parse();
    node->type(ctx);
    node->codegen(ctx);

    ctx.module_.dump();

    std::string err;
    llvm::raw_fd_ostream os((module_name + ".bc").c_str(), err);
    llvm::WriteBitcodeToFile(&ctx.module_, os);
    os.close();

    auto bc_file_name = module_name + ".bc";
    auto obj_file_name = module_name + ".o";
    auto exe_file_name = module_name + ".out";

    system(("llc-3.4 -filetype=obj " + bc_file_name + " -o " + obj_file_name).c_str());
    system("clang -std=c11 -O1 -Wall -Werror -c lib.c");
    system(("clang lib.o " + obj_file_name + " -o " + exe_file_name).c_str());

    return 0;
}
