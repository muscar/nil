#include <iostream>
#include <fstream>
#include <sstream>
#include <memory>
#include <exception>
#include <vector>
#include <map>
#include <algorithm>
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
    // Codegen

    struct codegen_ctx
    {
        llvm::Module module_;
        llvm::FunctionPassManager pass_mgr_;
        llvm::IRBuilder<> builder_;
        std::map<std::string, llvm::Value *> symtab_;

        codegen_ctx(const std::string &name)
        : module_{name, llvm::getGlobalContext()}, pass_mgr_{&module_}, builder_{llvm::getGlobalContext()}
        { }

        void init()
        {
            llvm::InitializeNativeTarget();

            pass_mgr_.add(llvm::createBasicAliasAnalysisPass());
            pass_mgr_.add(llvm::createInstructionCombiningPass());
            pass_mgr_.add(llvm::createReassociatePass());
            pass_mgr_.add(llvm::createGVNPass());
            pass_mgr_.add(llvm::createCFGSimplificationPass());

            pass_mgr_.doInitialization();
        }
    };

    // AST

    class expr_node
    {
    public:
        virtual ~expr_node() { }

        virtual llvm::Value *codegen(codegen_ctx &ctx) = 0;
    };

    class number_node : public expr_node
    {
        double value_;
    public:
        number_node(double value) : value_(value) { }

        double value() const
        {
            return value_;
        }

        llvm::Value *codegen(codegen_ctx &ctx)
        {
            return llvm::ConstantFP::get(llvm::getGlobalContext(), llvm::APFloat(value_));
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

        llvm::Value *codegen(codegen_ctx &ctx)
        {
            auto value = ctx.symtab_[name_];
            if (!value)
                throw std::domain_error("unbound variable");
            return value;
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


        llvm::Value *codegen(codegen_ctx &ctx)
        {
            auto f = ctx.module_.getFunction(target_);
            if (!f)
                throw std::domain_error("unknown function");
            if (f->arg_size() != args_.size())
                throw std::domain_error("argument count mismatch");
            std::vector<llvm::Value *> argvs(args_.size());
            std::transform(args_.begin(), args_.end(), argvs.begin(), [&](auto &&expr) { return expr->codegen(ctx); });

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

        llvm::Value *codegen(codegen_ctx &ctx)
        {
            // XXX not necessary to keep all values, just the last
            std::vector<llvm::Value *> values(exprs_.size());
            std::transform(exprs_.begin(), exprs_.end(), values.begin(), [&](auto &&expr) { return expr->codegen(ctx); });
            return values.back();
        }
    };

    class def_node
    {
    public:
        virtual ~def_node() { }

        virtual llvm::Function *codegen(codegen_ctx &ctx) = 0;
    };

    class prototype_node : public def_node
    {
        std::string name_;
        std::vector<std::string> params_;
    public:
        prototype_node(std::string name, std::vector<std::string> params)
        : name_(name), params_(params)
        { }

        llvm::Function *codegen(codegen_ctx &ctx)
        {
            auto double_ty = llvm::Type::getDoubleTy(llvm::getGlobalContext());
            std::vector<llvm::Type *> param_tys(params_.size(), double_ty);
            auto fun_ty = llvm::FunctionType::get(double_ty, param_tys, false);
            auto f = llvm::Function::Create(fun_ty, llvm::Function::ExternalLinkage, name_, &ctx.module_);
            
            if (f->getName() != name_) {
                f->eraseFromParent();
                f = ctx.module_.getFunction(name_);
            }
            
            if (!f->empty())
                throw std::domain_error("function redefinition");
            
            if (f->arg_size() != params_.size())
                throw std::domain_error("function redefinition with different number of params");

            auto idx = 0;
            for (auto iter = f->arg_begin(); idx != params_.size(); ++iter, ++idx) {
                iter->setName(params_[idx]);
                ctx.symtab_[params_[idx]] = iter;
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


        llvm::Function *codegen(codegen_ctx &ctx)
        {
            ctx.symtab_.clear();
            auto f = proto_->codegen(ctx);

            auto block = llvm::BasicBlock::Create(llvm::getGlobalContext(), "entry", f);
            ctx.builder_.SetInsertPoint(block);

            auto ret_val = body_->codegen(ctx);
            ctx.builder_.CreateRet(ret_val);

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

        void codegen(codegen_ctx &ctx)
        {
            for (auto &&def : defs_) {
                auto f = def->codegen(ctx);
                llvm::verifyFunction(*f);
                ctx.pass_mgr_.run(*f);
            }
        }
    };

    // Scanner

    enum class token
    {
        unknown = -2,
        eof = -1,
        ident = 0,
        num,
        kw_fun,
        kw_extern,
        lpar,
        rpar,
        lcurly,
        rcurly,
        comma,
        semicolon
    };

    std::string to_string(token tok)
    {
        switch (tok) {
            case token::unknown:
                return "unknown";
            case token::eof:
                return "end of file";
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
            case token::comma:
                return ",";
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
        } else if (in.peek() == ',') {
            tok = token::comma;
            in.get();
        } else if (in.peek() == ';') {
            tok = token::semicolon;
            in.get();
        } else if (isdigit(in.peek())) {
            read_while(in, isdigit);
            tok = token::num;
        } else if (isalnum(in.peek())) {
            read_while(in, [](auto c) { return isalnum(c) || c == '_'; });
            if (lexeme == "fun")
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
            while (curr_tok_ == token::kw_extern || curr_tok_ == token::kw_fun)
                mod->add_def(parse_def());
            return mod;
        }

        // def = fun_proto | fun_decl
        std::unique_ptr<def_node> parse_def()
        {
            switch (curr_tok_) {
                case token::kw_extern:
                    move_next();
                    return parse_prototype();
                case token::kw_fun:
                    return parse_function();
            }
            throw std::domain_error("expecting a definition at the toplevel");
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

        // fun_proto = "fun" ident "(" [ident {"," ident}] } ")"
        std::unique_ptr<prototype_node> parse_prototype()
        {
            expect(token::kw_fun);
            auto name = lexeme;
            move_next();

            expect(token::lpar);

            std::vector<std::string> params;

            if (curr_tok_ != token::rpar) {
                expect(token::ident);
                params.push_back(lexeme);
                while (curr_tok_ == token::comma) {
                    move_next();
                    expect(token::ident);
                    params.push_back(lexeme);
                }
            }

            expect(token::rpar);

            return std::make_unique<prototype_node>(name, std::move(params));
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
                    auto value = std::stod(lexeme);
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

            if (curr_tok_ != token::lpar)
                return std::make_unique<var_node>(name);

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

int main(int argc, const char *argv[])
{
    kaleidoscope::codegen_ctx ctx{"kaleidoscope"};
    ctx.init();

    std::ifstream in(argv[1]);
    kaleidoscope::parser p{in};

    auto node = p.parse();
    node->codegen(ctx);

    std::string err;
    llvm::raw_fd_ostream os("out.bc", err);
    llvm::WriteBitcodeToFile(&ctx.module_, os);

    return 0;
}
